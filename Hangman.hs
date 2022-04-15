module Hangman where

import Helpers
import Shape

data State = State String [Char] [Char]

hangman :: [String] -> IO ()
hangman words = randomPick words >>= newSession
       
newSession :: String -> IO ()
newSession word = printState $ State word [] []

printLine :: String -> IO ()
printLine s = putStr s >> fill (length s)

printAlphabet :: [Char] -> IO ()
printAlphabet missing = printLine $ alphabet missing

printWord :: [Char] -> IO ()
printWord w = printLine $ charJoin " " w

blankLine :: IO ()
blankLine = printLine ""

printState :: State -> IO ()
printState (State word valid invalid) = 
  printAlphabet (valid ++ invalid) >> printShapeRow 0  
    >> blankLine >> printShapeRow 1
    >> printWord word >> printShapeRow 2
    >> fill 0 >> printShapeRow 3
    >> fill 0 >> printShapeRow 4
    >> fill 0 >> printShapeRow 5
-- 
--   +----+
--   |    |
--   |    O 
--   |   /|\
--   |   / \
--   |
-- 


