module Hangman where

import System.Random
import Data.Char

hangman :: [String] -> IO ()
hangman words = randomPick words >>= session

randomPick :: [String] -> IO String
randomPick words = (words !!) <$> (randomRIO (0, end))
  where end = (length words) - 1

-- nextChar :: Char -> Char
-- nextChar = chr.(+1).ord
-- 
-- symbol hiden c
--   | elem c hiden = "  "
--   | otherwise = c : " "
-- 
-- azTable :: Char -> [Char] -> String
-- azTable 'Z' hidden = (symbol hidden 'Z')
-- azTable c hidden = (symbol hidden c) ++ azTable (nextChar c) hidden
-- 
-- alphabet :: [Char] -> String
-- alphabet = azTable 'A'

alphabet :: [Char] -> String
alphabet hidden = chr <$> [65..90]

session :: String -> IO ()
session word = putStrLn (alphabet ['G']) >> putStrLn word
