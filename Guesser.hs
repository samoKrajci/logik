module Guesser
  ( Game (..),
    newGame,
    play,
    roundsElapsedIncrease,
  )
where

import Common
  ( Code,
    Config (..),
    evaluateGuess,
    isCodeValid,
    isWinningAnswer,
    randomCode,
    userCreateConfig,
    validColors',
  )
import Data.List (sort)
import System.Random (StdGen, getStdGen, randomRs)

data Game = Game
  { config :: Config,
    password :: Code,
    roundsElapsed :: Int
  }

newGame :: StdGen -> Config -> Game
newGame gen c =
  Game
    { config = c,
      roundsElapsed = 0,
      password = pass
    }
  where
    pass = randomCode gen (plength c) (colors c)

getValidCode :: Config -> IO Code
getValidCode c = do
  guess <- getLine
  if isCodeValid c guess
    then return guess
    else do
      putStrLn "Invalid format. Try again:"
      getValidCode c

roundsElapsedIncrease :: Game -> Game
roundsElapsedIncrease g = g {roundsElapsed = roundsElapsed g + 1}

playGame :: Game -> IO ()
playGame g@Game {config = c, password = pass} = do
  putStrLn $ "[" ++ show (roundsElapsed g + 1) ++ "] Make a guess:"
  guess <- getValidCode c
  let answer = evaluateGuess pass guess
  if isWinningAnswer c answer
    then putStrLn $ "Congratulations! You guessed the correct password in " ++ show (roundsElapsed g + 1) ++ " guesses!"
    else do
      print answer
      playGame (roundsElapsedIncrease g)

play :: IO ()
play = do
  gen <- getStdGen
  game <- newGame gen <$> userCreateConfig
  playGame game