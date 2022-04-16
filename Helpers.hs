module Helpers where

import System.Random
import Data.Bool
import Data.Char

randomPick :: [String] -> IO String
randomPick words = (words !!) <$> (randomRIO (0, end))
  where end = (length words) - 1

charJoin :: char -> [Char] -> String
charJoin s c = mconcat $ (: " ") <$> c

alphabet :: [Char] -> String
alphabet hidden = charJoin " " $ get.chr <$> [97..122]
  where 
    get :: Char -> Char
    get c 
      | c `elem` hidden = ' '
      | otherwise = toUpper c

fill :: Int -> IO ()
fill x = putStr $ replicate (56 - x) ' '
