module Main where

import System.Directory
import System.FilePath()
import Data.List
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

mkTokens :: (Ord a) => [a] -> [((a,a),Float)]
mkTokens xs = M.toList (M.fromListWith (+) [(x,(1/(fromIntegral (length (xs))))) | x <- (zip xs (tail xs)) ] )

mkWord :: (Ord a) => [a] -> [(a,Float)]
mkWord xs = M.toList (M.fromListWith (+) [(x,(1/(fromIntegral (length (xs))))) | x <- (xs) ] )

listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + listSum xs

mkChain :: [(T.Text,T.Text)] -> Int -> [T.Text] -> [T.Text]
mkChain _ 1 chain = chain
mkChain tokenList lc chain
                        | ((filter (\tl -> (fst $ tl)==(last chain)) tokenList))==[]=( mkChain (filter (\tl -> (fst $ tl)/=(last chain)) tokenList) (lc-1) ( chain++[(fst $ tokenList!!0)] ) )
                        | otherwise = mkChain (filter (\tl -> (fst $ tl)/=(last chain)) tokenList) (lc-1) ( chain++[snd $ (filter (\tl -> (fst $ tl)==(last chain)) tokenList)!!0 ] )

fstWord :: [T.Text] -> T.Text
fstWord wList = fst $ (sortBy (\(_,a) (_,b) -> compare b a) (mkWord $ concat $ map (\ln -> map (\x -> (T.words x)!!0) (T.lines ln)) wList))!!0

main :: IO ()
main = do
        filesDir <- (++) <$> getCurrentDirectory <*> pure "/data"
        filesNames <- filter (\x -> isInfixOf ".txt" x) <$> getDirectoryContents filesDir
        filesData <- mapM T.readFile (map ("data/"++) filesNames)
        let wordBag = map (T.words <$> T.map (\x -> if isAlphaNum x then x else ' ') <$> T.toLower) filesData
        let filesLen = map length (mkTokens <$> wordBag)
        let avgLen = listSum filesLen `div` length filesLen
        let sorted = sortBy (\(_,a) (_,b) -> compare b a) (mkTokens $ concat wordBag)
        T.writeFile "out.txt" (T.unwords ( mkChain (map (fst) sorted) avgLen [( fstWord filesData )] ))
