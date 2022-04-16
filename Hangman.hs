module Hangman where

import Data.Char
import Control.Monad.Trans
import System.Console.Haskeline

import Helpers
import Shape

data State = State String [Char] [Char]

hangman :: [String] -> IO ()
hangman words = randomPick words >>= newSession
       
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
    >> blankLine >> pRow 3
    >> blankLine >> pRow 4
    >> blankLine >> pRow 5
  where pRow r = printShapeRow r $ length invalid

newSession :: String -> IO ()
newSession word = stateLoop $ State word "" ""

progress :: Char -> State -> State
progress c (State word valid invalid) 
  | c `elem` word = State word (c : valid) invalid
  | otherwise = State word valid (c : invalid)

stateLoop :: State -> IO ()
stateLoop state = runInputT defaultSettings (loop state)
  where 
    loop :: State -> InputT IO ()
    loop s = do
      lift (printState s)
      key <- getInputChar "Enter a char: "
      case key of
        Nothing -> return ()
        Just k -> do
          loop $ progress (toLower k) s
          -- lift $ putChar k
          loop s
