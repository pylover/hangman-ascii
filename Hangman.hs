
module Hangman where

import System.Random

hangman :: [String] -> IO ()
hangman words = session =<< randomPick words

randomPick :: [String] -> IO String
randomPick words = (words !!) <$> (randomRIO (0, end))
  where end = (length words) - 1

session :: String -> IO ()
session word = putStrLn word
