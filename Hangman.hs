module Hangman where

import Data.Char
import Control.Monad.Trans
import System.Console.Haskeline

import Helpers
import Shape

data Status 
  = New
  | Playing
  | Failed
  | Success
  deriving (Show)

data State = State Status String [Char] [Char]

hangman :: [String] -> IO ()
hangman words = do
  w <- randomPick words
  putStrLn w
  stateLoop (State New w [] [])

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
printState (State New word valid invalid) = do
    printAlphabet (valid ++ invalid) >> pRow 0  
    blankLine >> pRow 1
    printWord word valid >> pRow 2
    blankLine >> pRow 3
    blankLine >> pRow 4
    blankLine >> pRow 5
  where pRow r = printShapeRow r $ length invalid
printState (State _ word valid invalid) = do
  -- ANSI: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
  putStr "\x1b[7A\x1b[0J"
  printState $ State New word valid invalid

message :: State -> String
message (State Failed _ _ _) = 
  "Failed! Press any key to continue, CTRL+D to exit: "
message (State Success _ _ _) = 
  "**You Won** Press any key to continue, CTRL+D to exit: "
message _ = 
  "Enter a character from A to Z: "

progress :: Maybe Char -> State -> Maybe State
progress Nothing _ = Nothing
progress (Just c) (State status word valid invalid) 
  | c `elem` word = Just $ State Playing word ((toLower c) : valid) invalid
  | otherwise = Just $ State Playing word valid ((toLower c) : invalid)


stateLoop :: State -> IO ()
stateLoop state = runInputT defaultSettings (loop state)
  where 
    loop :: State -> InputT IO ()
    loop s = do
      lift (printState s)
      c <- getInputChar $ message s
      case progress c s of
        Nothing -> return ()
        Just ns -> loop ns
