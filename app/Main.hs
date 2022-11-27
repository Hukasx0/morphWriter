module Main where

import System.Directory
import System.FilePath()
import Data.List
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

mkTokens :: (Ord a) => [a] -> [(a,Float)]
mkTokens xs = M.toList (M.fromListWith (+) [(x,(1/(fromIntegral (length xs)))) | x <- xs])

listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + listSum xs

main :: IO ()
main = do
        filesDir <- (++) <$> getCurrentDirectory <*> pure "/data"
        filesNames <- filter (\x -> isInfixOf ".txt" x) <$> getDirectoryContents filesDir
        filesData <- mapM T.readFile (map ("data/"++) filesNames)
        let wordBag = map (T.words <$> T.map (\x -> if isAlphaNum x then x else ' ') <$> T.toLower) filesData
        let filesLen = map length (mkTokens <$> wordBag)
        let avgLen = listSum filesLen `div` length filesLen
        let sorted = sortBy (\(_,a) (_,b) -> compare b a) (mkTokens $ concat wordBag)
        T.writeFile "out.txt" (T.unwords (map fst (take avgLen sorted)))