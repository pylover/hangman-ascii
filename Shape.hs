module Hangman.Shape where

import Data.Bool

data Pixel = Pixel 
  { pSeverity :: Int
  , pRow :: Int 
  , pColumn :: Int 
  , pSymbol :: Char
  }
  deriving (Show)

--       
--       
--     \O/
--      | 
--     / \
--  _________
-- |_________|

shapeWon :: [Pixel]
shapeWon = 
  [ Pixel 0 2 5 'O'
  , Pixel 0 2 6 '/'
  , Pixel 0 3 5 '|'
  , Pixel 0 2 4 '\\'
  , Pixel 0 4 4 '/'
  , Pixel 0 4 6 '\\'

  , Pixel 0 5 1 '_' 
  , Pixel 0 5 2 '_' 
  , Pixel 0 5 3 '_' 
  , Pixel 0 5 4 '_' 
  , Pixel 0 5 5 '_' 
  , Pixel 0 5 6 '_' 
  , Pixel 0 5 7 '_' 
  , Pixel 0 5 8 '_' 
  , Pixel 0 5 9 '_' 

  , Pixel 0 6 0 '|' 
  , Pixel 0 6 1 '_' 
  , Pixel 0 6 2 '_' 
  , Pixel 0 6 3 '_' 
  , Pixel 0 6 4 '_' 
  , Pixel 0 6 5 '_' 
  , Pixel 0 6 6 '_' 
  , Pixel 0 6 7 '_' 
  , Pixel 0 6 8 '_' 
  , Pixel 0 6 9 '_' 
  , Pixel 0 6 10 '|' 
  ]

--   +----+
--   |    |
--   |    O 
--   |   /|\
--   |   / \
--  _|_______
-- |_________|

shapeHangman :: [Pixel]
shapeHangman = 
  [ Pixel  6 0 2 '+'
  , Pixel  7 0 3 '-'
  , Pixel  8 0 4 '-'
  , Pixel  9 0 5 '-'
  , Pixel 10 0 6 '-'
  , Pixel 11 0 7 '+'

  , Pixel  5 1 2 '|'
  , Pixel 12 1 7 '|'

  , Pixel  4 2 2 '|'
  , Pixel 13 2 7 'O'

  , Pixel  3 3 2 '|'
  , Pixel 14 3 6 '/'
  , Pixel 15 3 7 '|'
  , Pixel 16 3 8 '\\'

  , Pixel  2 4 2 '|'
  , Pixel 17 4 6 '/'
  , Pixel 18 4 8 '\\'

  , Pixel  1 5 1 '_' 
  , Pixel  1 5 2 '|' 
  , Pixel  1 5 3 '_' 
  , Pixel  1 5 4 '_' 
  , Pixel  1 5 5 '_' 
  , Pixel  1 5 6 '_' 
  , Pixel  1 5 7 '_' 
  , Pixel  1 5 8 '_' 
  , Pixel  1 5 9 '_' 

  , Pixel  1 6 0 '|' 
  , Pixel  1 6 1 '_' 
  , Pixel  1 6 2 '_' 
  , Pixel  1 6 3 '_' 
  , Pixel  1 6 4 '_' 
  , Pixel  1 6 5 '_' 
  , Pixel  1 6 6 '_' 
  , Pixel  1 6 7 '_' 
  , Pixel  1 6 8 '_' 
  , Pixel  1 6 9 '_' 
  , Pixel  1 6 10 '|' 
  ]

shapeCharAt :: [Pixel] -> Int -> Int -> Int -> Char
shapeCharAt [] _ _ _ = ' '
shapeCharAt (x:xs) v r c  
  | (r == pRow x) && (c == pColumn x) = 
      bool (pSymbol x) ' ' (v < (pSeverity x))
  | otherwise = shapeCharAt xs v r c

shapeColumns :: [Pixel] -> Int
shapeColumns s = maximum $ pColumn <$> s

shapeRows :: [Pixel] -> Int
shapeRows s = maximum $ pRow <$> s

shapePixels :: [Pixel] -> Int -> Int -> Int -> Int -> Int -> String
shapePixels shape v row col maxRow maxCol
  | row > maxRow = ""
  | col == maxCol = c : shapePixels shape v (row + 1) 0 maxRow maxCol
  | otherwise = c: shapePixels shape v row (col + 1) maxRow maxCol
  where c = shapeCharAt shape v row col

shapeRow :: [Pixel] -> Int -> Int -> String
shapeRow s row v = shapePixels s v row 0 row (shapeColumns s)

shapeHangmanRow = shapeRow shapeHangman
shapeWonRow = shapeRow shapeWon

printShape :: Int -> IO ()
printShape v = putStrLn $ shapePixels s v 0 0 (shapeRows s) (shapeColumns s)
  where s = shapeHangman
