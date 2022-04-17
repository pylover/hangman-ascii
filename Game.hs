module Hangman.Game where

import Data.Set
import Data.Char
import Data.Bool
import Control.Monad.Trans
import System.Console.Haskeline
import System.Random

import Hangman.Shape

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
data Session = Session Int Int
data Game = Game String String Skill (Set Char) (Set Char)
data Status = Playing | GameOver| Won

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

printState :: Session -> Status -> Game -> IO ()
printState (Session wins fails) st (Game word cat skill valid invalid) = do
  putStr $ "\x1b[" ++ (show rows) ++ "A\x1b[0J"
  mconcat $ putStrLn <$> zipWith fill
    [ "Wins: " ++ (show wins) ++ " Fails: " ++ (show fails)
    , mconcat $ (:" ") <$> alphabet (valid `union` invalid)
    , ""
    , mconcat $ (:" ").wordChar <$> word
    , ""
    -- , word
    , hint st
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
    hint GameOver = mconcat $ (:" ") <$> word
    hint _ = "Category: " ++ cat

progress :: Char -> Game -> Game
progress c (Game word cat skill valid invalid) 
  | c `elem` word = Game word cat skill (insert cl valid) invalid
  | otherwise     = Game word cat skill valid (insert cl invalid)
  where cl = toLower c

status :: Game -> Status
status (Game word cat skill valid invalid) 
  | (length invalid) == (maxMistakes skill) = GameOver
  | (fromList word) == valid = Won
  | otherwise = Playing

stateLoop :: Session -> Game -> InputT IO Bool 
stateLoop s g = do
  let st = status g
  lift (printState s st g)
  case st of
    GameOver -> outputStrLn "Game Over!" >> return False
    Won -> outputStrLn "You won !!!" >> return True
    Playing -> do
      key <- getInputChar "Guess a character, a ~ z: "
      case key of
        Nothing -> return False 
        Just c -> stateLoop s (progress c g)

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

updateSession :: Bool -> Session -> Session
updateSession True (Session wins fails) = Session (wins + 1) fails
updateSession False (Session wins fails) = Session wins (fails + 1)

sessionLoop :: Session -> String -> [String] -> Skill -> InputT IO ()
sessionLoop session cat words skill = do
  w <- lift (randomPick words)
  won <- stateLoop session (Game w cat skill empty empty)
  c <- continue
  case c of
    False -> return ()
    True -> do
      outputStrLn "\x1b[2A\x1b[0J"
      sessionLoop (updateSession won session) cat words skill

hangman :: String -> [String] -> Skill -> IO ()
hangman cat words skill = do
  mconcat $ replicate rows (putStrLn "") 
  runInputT defaultSettings $ sessionLoop (Session 0 0) cat words skill
