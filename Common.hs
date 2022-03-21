module Common where

import Data.Set (fromList, toList)
import System.Random (StdGen, getStdGen, randomRs)

type Color = Char

type Code = [Color]

data Config = Config
  { colors :: Int,
    plength :: Int
  }

instance Show Config where
  show (Config c l) =
    "colors: "
      ++ show c
      ++ "\nPasswordLength: "
      ++ show l

data Answer = Answer
  { red :: Int,
    yellow :: Int
  }
  deriving (Eq)

instance Show Answer where
  show (Answer r y) = show r ++ " red, " ++ show y ++ " yellow"

userCreateConfig :: IO Config
userCreateConfig = do
  putStrLn "Choose the amount of colors (letters) [1-26]:"
  c <- (read :: String -> Int) <$> getLine
  putStrLn "Choose the length of the password:"
  l <- (read :: String -> Int) <$> getLine
  return Config {colors = c, plength = l}

randomCode :: StdGen -> Int -> Int -> Code
randomCode gen length colors = take length $ randomRs (head range, last range) gen :: [Char]
  where
    range = validColors' colors

validColors' :: Int -> [Color]
validColors' colors = take colors ['a' .. 'z']

validColors :: Config -> [Color]
validColors c = validColors' (colors c)

isWinningAnswer :: Config -> Answer -> Bool
isWinningAnswer s a = plength s == red a

isCodeValid :: Config -> Code -> Bool
isCodeValid config code = lengthOk && colorsOk
  where
    lengthOk = plength config == length code
    colorsOk = all (`elem` validColors config) code

isAnswerValid :: Config -> Answer -> Bool
isAnswerValid c Answer {red = red, yellow = yellow} = (red >= 0) && (yellow >= 0) && (red + yellow <= plength c)

evaluateGuess :: Code -> Code -> Answer
evaluateGuess pass guess = Answer (nred guess pass) (nyellow guess pass)
  where
    nred a b = length . filter (uncurry (==)) $ zip a b
    nyellow a b = sum (map (\elem -> min (noccurences elem a) (noccurences elem b)) (unique a)) - nred a b
    unique = toList . fromList
    noccurences elem list = length . filter (== elem) $ list

dummyConfig :: Config
dummyConfig =
  Config
    { colors = 3,
      plength = 4
    }