module Hangman2.Game where

import Data.Set
import Data.Char
import Data.Bool
import Control.Monad.Trans
import System.Console.Haskeline
import System.Random

import Hangman2.Shape

rows :: Int
rows = 8

maxMistakes :: Skill -> Int
maxMistakes Easy = 18
maxMistakes Medium = 10
maxMistakes Hard = 6

maxShapeProgress :: Int
maxShapeProgress = maximum $ pSeverity <$> shapeHangman 


shapeProgress :: Skill -> Int -> Int
shapeProgress Easy invalids = invalids 
shapeProgress s invalids = invalids * maxShapeProgress `div` (maxMistakes s)


data Skill = Easy | Medium | Hard

data Game = Game String Skill (Set Char) (Set Char)

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
printState st (Game word skill valid invalid) = do
  putStrLn "\x1b[8A\x1b[0J"
  mconcat $ putStrLn <$> zipWith fill
    [ mconcat $ (:" ") <$> alphabet (valid `union` invalid)
    , ""
    , mconcat $ (:" ").wordChar <$> word
    , ""
    -- , word
    , getWord st
    , ""
    ] (shape st <$> [0..6])
  where 
    fill x y = x ++ replicate (56 - length x) ' ' ++ y
    errLen = length invalid
    shape Won r = shapeWonRow r 0
    shape _ r = shapeHangmanRow r $ shapeProgress skill errLen
    wordChar c 
      | c `member` valid = c
      | otherwise = '_'
    getWord GameOver = mconcat $ (:" ") <$> word
    getWord _ = ""

progress :: Char -> Game -> Game
progress c (Game word skill valid invalid) 
  | c `elem` word = Game word skill (insert cl valid) invalid
  | otherwise     = Game word skill valid (insert cl invalid)
  where cl = toLower c

status :: Game -> Status
status (Game word skill valid invalid) 
  | (length invalid) == (maxMistakes skill) = GameOver
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

continue :: InputT IO Bool
continue = do
  key <- getInputChar "Do you want to continue? [Y/n] "
  case toLower <$> key of
    Nothing -> return False
    Just 'n' -> return False
    Just 'y' -> return True
    Just _ -> do 
      outputStr "\x1b[1A\x1b[0G"
      continue

sessionLoop :: [String] -> Skill -> InputT IO ()
sessionLoop words skill = do
  w <- lift (randomPick words)
  stateLoop (Game w skill empty empty)
  c <- continue
  case c of
    False -> return ()
    True -> do
      outputStrLn "\x1b[2A\x1b[0J"
      sessionLoop words skill

hangman :: [String] -> Skill -> IO ()
hangman words skill = do
  mconcat $ replicate rows (putStrLn "") 
  runInputT defaultSettings $ sessionLoop words skill
