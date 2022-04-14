module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data Args = Args
  { quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Args
sample = Args
  <$> switch
    ( long "quiet"
   <> short 'q'
   <> help "Whether to be quiet" )
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

greet :: Args -> IO ()
greet (Args False n) = putStrLn $ "Hello, " ++ replicate n '!'
greet _ = return ()
