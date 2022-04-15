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
alphabet hidden = charJoin " " $ get.chr <$> [65..90]
  where get c = bool c ' ' (c `elem` hidden)

fill :: Int -> IO ()
fill x = putStr $ replicate (56 - x) ' '
