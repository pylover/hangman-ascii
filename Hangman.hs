module Hangman where

import Data.Set
import Data.Char
import Control.Monad.Trans
import System.Console.Haskeline

import Helpers
import Shape

rows :: Int
rows = 8

maxMistakes :: Int
maxMistakes = 18 

data State = State String (Set Char) (Set Char)

data Status 
  = Playing
  | GameOver
  | Won


printLine :: String -> IO ()
printLine s = putStr s >> fill (length s)

printAlphabet :: Set Char -> IO ()
printAlphabet missing = printLine $ alphabet missing

wordChar :: Set Char -> Char -> Char
wordChar valid c 
  | c `member` valid = c
  | otherwise = '_'

printWord :: String -> Set Char -> IO ()
printWord w v = printLine $ charJoin " " (get <$> w)
  where get = wordChar v

blankLine :: IO ()
blankLine = printLine ""

printState :: State -> IO ()
printState (State word valid invalid) = do
  -- ANSI: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
  putStrLn "\x1b[8A\x1b[0J"
  --putStrLn word
  printAlphabet (valid `union` invalid) >> pRow 0  
  blankLine >> pRow 1
  printWord word valid >> pRow 2
  blankLine >> pRow 3
  printLine word >> pRow 4
  blankLine >> pRow 5
  where pRow r = printShapeRow r $ length invalid

progress :: Char -> State -> State
progress c (State word valid invalid) 
  | c `elem` word = State word (insert cl valid) invalid
  | otherwise     = State word valid (insert cl invalid)
  where cl = toLower c

status :: State -> Status
status (State word valid invalid) 
  | (length invalid) == maxMistakes = GameOver
  | (fromList word) == valid = Won
  | otherwise = Playing

stateLoop :: State -> InputT IO ()
stateLoop s = do
  lift (printState s)
  case status s of
    GameOver -> outputStrLn "Game Over!" >> return ()
    Won -> outputStrLn "You won !!!" >> return ()
    Playing -> do
      key <- getInputChar "Guess a character, a ~ z: "
      case key of
        Nothing -> return ()
        Just c -> stateLoop (progress c s)

hangman :: [String] -> IO ()
hangman words = do
  mconcat $ replicate rows (putStrLn "") 
  runInputT defaultSettings loop
  where 
    loop :: InputT IO ()
    loop = do
      w <- lift (randomPick words)
      stateLoop (State w empty empty)
      key <- getInputChar "Do you want to continue? [Y/n] "
      case key of
        Nothing -> return ()
        Just 'n' -> return ()
        Just _ -> do
          lift (putStrLn "\x1b[2A\x1b[0J")
          loop
