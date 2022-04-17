module Shape where

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
--    \O/
--     |
--    / \
--

shapeWon :: [Pixel]
shapeWon = 
  [ Pixel 0 2 4 '\\'
  , Pixel 0 2 5 'O'
  , Pixel 0 2 6 '/'
  , Pixel 0 3 5 '|'
  , Pixel 0 4 4 '/'
  , Pixel 0 4 6 '\\'

  ]


--   +----+
--   |    |
--   |    O 
--   |   /|\
--   |   / \
--   |
shapeHangman :: [Pixel]
shapeHangman = 
  [ Pixel  6 0 0 '+'
  , Pixel  7 0 1 '-'
  , Pixel  8 0 2 '-'
  , Pixel  9 0 3 '-'
  , Pixel 10 0 4 '-'
  , Pixel 11 0 5 '+'

  , Pixel  5 1 0 '|'
  , Pixel 12 1 5 '|'

  , Pixel  4 2 0 '|'
  , Pixel 13 2 5 'O'

  , Pixel  3 3 0 '|'
  , Pixel 14 3 4 '/'
  , Pixel 15 3 5 '|'
  , Pixel 16 3 6 '\\'

  , Pixel  2 4 0 '|'
  , Pixel 17 4 4 '/'
  , Pixel 18 4 6 '\\'

  , Pixel  1 5 0 '|' 
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
