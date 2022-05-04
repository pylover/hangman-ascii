module Main where

import System.IO
import Text.Printf
import Options.Applicative
import Data.Semigroup ((<>))

import HangmanAscii.Game
import HangmanAscii.Database
import Paths_HangmanAscii

data Args = Args
  { dbfile :: String
  , skill :: String 
  , category :: String
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
   <> help "easy, medium, hard."
   <> showDefault
   <> value "medium"
   <> metavar "SKILL" )
  <*> strOption 
    ( long "category"
   <> short 'c'
   <> help ("Categories: all, " ++ conCat ++ ".")
   <> showDefault
   <> value "fruites"
   <> metavar "SKILL" )


parseArgs :: IO Args
parseArgs = execParser opts
  where
    opts = info (sample <**> helper) (progDesc "Hangman game")

loadWords :: String -> String -> IO [String]
loadWords "" category = return $ getWords category
loadWords fn _ = fmap lines $ readFile fn

getSkill :: String -> Skill
getSkill "easy" = Easy
getSkill "medium" = Medium
getSkill "hard" = Hard

main :: IO ()
main = parseArgs >>= go
  where 
    go :: Args -> IO ()
    go (Args dbfile skl category) = do
      words <- loadWords dbfile category
      printf "Total words: %d\r\n" (length words)
      hangman category words (getSkill skl)
