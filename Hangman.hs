module Hangman where

import System.Random
import Data.Char
import Data.Bool
import Data.List

hangman :: [String] -> IO ()
hangman words = randomPick words >>= session

randomPick :: [String] -> IO String
randomPick words = (words !!) <$> (randomRIO (0, end))
  where end = (length words) - 1

alphabet :: [Char] -> String
alphabet hidden = mconcat $ get.chr <$> [65..90]
  where get c = bool c ' ' (c `elem` hidden) : " "
        
session :: String -> IO ()
session word = putStrLn (alphabet ['G']) >> putStrLn word
