module Main where

import Text.Printf
import Options.Applicative
import Data.Semigroup ((<>))

import Hangman

--   +----+
--   |    |
--   |    O 
--   |   /|\
--   |   / \
--   |

data Args = Args
  { dbfile     :: String
  , enthusiasm :: Int }

sample :: Parser Args
sample = Args
  <$> option auto
    ( long "dbfile"
   <> help "Text file, one word per line"
   <> value "db.txt"
   <> metavar "FILENAME" )
  <*> option auto
    ( long "enthusiasm"
   <> help "How enthusiastically to greet"
   <> showDefault
   <> value 1
   <> metavar "INT" )

parseArgs :: IO Args
parseArgs = execParser opts
  where
    opts = info (sample <**> helper) (progDesc "Hangman game")

main :: IO ()
main = do
  greet =<< parseArgs

loadWords :: String -> IO [String]
loadWords fn = fmap lines $ readFile fn

greet :: Args -> IO ()
greet (Args dbfile n) = do
  words <- loadWords dbfile
  printf "Total words: %d\r\n" (length words)
  hangman words

