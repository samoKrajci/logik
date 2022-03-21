module Main where

import Answerer as A
import Frontend as F
import Guesser as G
import System.Environment (getArgs)

helpMessage :: String
helpMessage =
  "Specify game type:\n \
  \ \t-g\tguesser\n\
  \ \t-gui\tguesser with GUI\n\
  \ \t-a\tanswerer"

mainWithArgs :: [String] -> IO ()
mainWithArgs ("-g" : _) = G.play
mainWithArgs ("-a" : _) = A.play
mainWithArgs ("-gui" : _) = F.run
mainWithArgs _ = putStrLn helpMessage

main :: IO ()
main = do
  args <- getArgs
  mainWithArgs args
