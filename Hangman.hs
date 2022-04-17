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
maxMistakes = maximum $ pSeverity <$> shapeHangman 

data State = State String (Set Char) (Set Char)

data Status 
  = Playing
  | GameOver
  | Won

printState :: Status -> State -> IO ()
printState st (State word valid invalid) = do
  putStrLn "\x1b[8A\x1b[0J"
  mconcat $ putStrLn <$> zipWith fill
    [ mconcat $ (:" ") <$> alphabet (valid `union` invalid)
    , ""
    , mconcat $ (:" ").wordChar <$> word
    , ""
    , word
    , ""
    ] (shape st <$> [0..6])
  where 
    fill x y = x ++ replicate (56 - length x) ' ' ++ y
    errLen = length invalid
    shape Won r = shapeWonRow r errLen
    shape _ r = shapeHangmanRow r errLen
    wordChar c 
      | c `member` valid = c
      | otherwise = '_'


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
  let st = status s
  lift (printState st s)
  case st of
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
          outputStrLn "\x1b[2A\x1b[0J"
          loop
