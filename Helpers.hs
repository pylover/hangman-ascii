module Helpers where

import System.Random
import Data.Bool
import Data.Set
import Data.Char

randomPick :: [String] -> IO String
randomPick words = (words !!) <$> (randomRIO (0, end))
  where end = (length words) - 1

alphabet :: Set Char -> String
alphabet hidden = get.chr <$> [97..122]
  where 
    get :: Char -> Char
    get c 
      | c `member` hidden = ' '
      | otherwise = toUpper c
