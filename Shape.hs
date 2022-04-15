module Shape where

data Pixel = Pixel 
  { pRow :: Int 
  , pColumn :: Int 
  , pSymbol :: Char
  }
  deriving (Show)

shapeHangman :: [Pixel]
shapeHangman = 
  [ Pixel 0 0 '+'
  , Pixel 0 1 '-'
  , Pixel 0 2 '-'
  , Pixel 0 3 '-'
  , Pixel 0 4 '-'
  , Pixel 0 5 '+'

  , Pixel 1 0 '|'
  , Pixel 1 5 '|'

  , Pixel 2 0 '|'
  , Pixel 2 5 'O'

  , Pixel 3 0 '|'
  , Pixel 3 4 '/'
  , Pixel 3 5 '|'
  , Pixel 3 6 '\\'

  , Pixel 4 0 '|'
  , Pixel 4 4 '/'
  , Pixel 4 6 '\\'

  , Pixel 5 0 '|' 
  ]

shapeCharAt :: [Pixel] -> Int -> Int -> Char
shapeCharAt [] _ _ = ' '
shapeCharAt (x:xs) r c  
  | (r == pRow x) && (c == pColumn x) = pSymbol x 
  | otherwise = shapeCharAt xs r c

shapeColumns :: [Pixel] -> Int
shapeColumns s = maximum $ pColumn <$> s

shapeRows :: [Pixel] -> Int
shapeRows s = maximum $ pRow <$> s

printShapePixels :: [Pixel] -> Int -> Int -> Int -> Int -> IO ()
printShapePixels shape row col maxRow maxCol
  | row > maxRow = putStr ""
  | col == maxCol = putChar c
      >> putStrLn ""
      >> printShapePixels shape (row + 1) 0 maxRow maxCol
  | otherwise =
      putChar c >> printShapePixels shape row (col + 1) maxRow maxCol
  where c = shapeCharAt shape row col

printShapeRow :: Int -> IO ()
printShapeRow row = printShapePixels s row 0 row (shapeColumns s)
  where s = shapeHangman

printShape :: IO ()
printShape = printShapePixels s 0 0 (shapeRows s) (shapeColumns s)
  where s = shapeHangman
