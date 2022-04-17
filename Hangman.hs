module Hangman where

import Data.Set
import Data.Char
import Data.Bool
import Control.Monad.Trans
import System.Console.Haskeline
import System.Random

import Shape

rows :: Int
rows = 8

maxMistakes :: Int
maxMistakes = maximum $ pSeverity <$> shapeHangman 

data Game = Game String (Set Char) (Set Char)

data Status 
  = Playing
  | GameOver
  | Won

randomPick :: [String] -> IO String
randomPick words = (words !!) <$> (randomRIO (0, end))
  where end = (length words) - 1

alphabet :: Set Char -> String
alphabet hidden = get.chr <$> [97..122]
  where 
    get :: Char -> Char
    get c 
      | c `member` hidden = ' '
      | otherwise = toUpper c

printState :: Status -> Game -> IO ()
printState st (Game word valid invalid) = do
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


progress :: Char -> Game -> Game
progress c (Game word valid invalid) 
  | c `elem` word = Game word (insert cl valid) invalid
  | otherwise     = Game word valid (insert cl invalid)
  where cl = toLower c

status :: Game -> Status
status (Game word valid invalid) 
  | (length invalid) == maxMistakes = GameOver
  | (fromList word) == valid = Won
  | otherwise = Playing

stateLoop :: Game -> InputT IO ()
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

sessionLoop ::[String] -> InputT IO ()
sessionLoop words= do
  w <- lift (randomPick words)
  stateLoop (Game w empty empty)
  key <- getInputChar "Do you want to continue? [Y/n] "
  case key of
    Nothing -> return ()
    Just 'n' -> return ()
    Just 'N' -> return ()
    Just _ -> do
      outputStrLn "\x1b[2A\x1b[0J"
      sessionLoop words

hangman :: [String] -> IO ()
hangman words = do
  mconcat $ replicate rows (putStrLn "") 
  runInputT defaultSettings $ sessionLoop words
