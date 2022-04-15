module Hangman where

import Helpers
import Shape

data State = State String [Char] [Char]

hangman :: [String] -> IO ()
hangman words = randomPick words >>= newSession
       
newSession :: String -> IO ()
newSession word = printState $ State word "" ""

printLine :: String -> IO ()
printLine s = putStr s >> fill (length s)

printAlphabet :: [Char] -> IO ()
printAlphabet missing = printLine $ alphabet missing

wordChar :: [Char] -> Char -> Char
wordChar valid c 
  | c `elem` valid = c
  | otherwise = '_'

printWord :: [Char] -> [Char] -> IO ()
printWord w v = printLine $ charJoin " " (get <$> w)
  where get = wordChar v

blankLine :: IO ()
blankLine = printLine ""

printState :: State -> IO ()
printState (State word valid invalid) = 
    putStrLn word
    >> printAlphabet (valid ++ invalid) >> pRow 0  
    >> blankLine >> pRow 1
    >> printWord word valid >> pRow 2
    >> fill 0 >> pRow 3
    >> fill 0 >> pRow 4
    >> fill 0 >> pRow 5
  where pRow r = printShapeRow r $ length invalid
