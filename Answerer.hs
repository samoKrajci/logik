module Answerer
  ( Game (..),
    play,
  )
where

import Common
  ( Answer (..),
    Code,
    Config (..),
    dummyConfig,
    evaluateGuess,
    isAnswerValid,
    isWinningAnswer,
    userCreateConfig,
    validColors,
  )
import Control.Monad (replicateM)
import System.Random (StdGen, getStdGen, randomR)

data Game = Game
  { config :: Config,
    roundsElapsed :: Int,
    availableCodes :: [Code]
  }

newGame :: Config -> Game
newGame c =
  Game
    { config = c,
      roundsElapsed = 0,
      availableCodes = allCodes c
    }

allCodes :: Config -> [Code]
allCodes c = replicateM (plength c) colors
  where
    colors = validColors c

isAnswerOK :: Code -> Answer -> Code -> Bool
isAnswerOK guess ans pass = ans == actualAnswer
  where
    actualAnswer = evaluateGuess pass guess

filterCodesByAnswer :: Code -> Answer -> [Code] -> [Code]
filterCodesByAnswer guess ans = filter $ isAnswerOK guess ans

makeGuess :: StdGen -> [Code] -> Code
makeGuess gen codes = codes !! fst (randomR (0, length codes - 1) gen)

getValidAnswer :: Config -> IO Answer
getValidAnswer c = do
  putStrLn "Reds:"
  red <- (read :: String -> Int) <$> getLine
  putStrLn "Yellows:"
  yellow <- (read :: String -> Int) <$> getLine
  let ans = Answer {red = red, yellow = yellow}
  if isAnswerValid c ans
    then return ans
    else do
      putStrLn "Are you sure? I don't believe you."
      getValidAnswer c

roundsElapsedIncrease :: Game -> Game
roundsElapsedIncrease g = g {roundsElapsed = roundsElapsed g + 1}

playGame :: Game -> IO ()
playGame g@Game {config = c, availableCodes = [code]} =
  putStrLn $ "I know! Your code is " ++ code
playGame g@Game {config = c, availableCodes = []} =
  putStrLn "Liar! Your answers make no sense!"
playGame g@Game {config = c, availableCodes = codes} =
  do
    gen <- getStdGen
    let guess = makeGuess gen codes
    putStrLn $ "My guess is:\n" ++ guess
    answer <- getValidAnswer c
    let filteredCodes = filterCodesByAnswer guess answer codes
    playGame (roundsElapsedIncrease g) {availableCodes = filteredCodes}

play :: IO ()
play = do
  config <- userCreateConfig
  let game = newGame config
  playGame game
