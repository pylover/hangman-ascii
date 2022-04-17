module Main where

import System.IO
import Text.Printf
import Options.Applicative
import Data.Semigroup ((<>))

import Hangman2.Game
import Hangman2.Database
import Paths_hangman2

data Args = Args
  { dbfile :: String
  , skill :: String 
  }

sample :: Parser Args
sample = Args
  <$> strOption 
    ( long "dbfile"
   <> help "Text file, one word per line"
   <> value ""
   <> metavar "FILENAME" )
  <*> strOption 
    ( long "skill"
   <> short 's'
   <> help "easy, medium, hard, default: medium"
   <> showDefault
   <> value "medium"
   <> metavar "SKILL" )

parseArgs :: IO Args
parseArgs = execParser opts
  where
    opts = info (sample <**> helper) (progDesc "Hangman game")

loadWords :: String -> IO [String]
loadWords "" = return dbWords
loadWords fn = fmap lines $ readFile fn

getSkill :: String -> Skill
getSkill "easy" = Easy
getSkill "medium" = Medium
getSkill "hard" = Hard

main :: IO ()
main = parseArgs >>= go
  where 
    go :: Args -> IO ()
    go (Args dbfile s) = do
      words <- loadWords dbfile
      printf "Total words: %d\r\n" (length words)
      hangman words $ getSkill s
