module Frontend where

import qualified Common as C
import Data.Char
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import qualified Guesser as G
import System.Random (StdGen, getStdGen)

type Coords = (Float, Float)

type Round = (C.Code, C.Answer)

data GameState = InProgress | Won | Lost deriving (Eq)

data World = World
  { game :: G.Game,
    curent :: C.Code,
    past :: [(C.Code, C.Answer)],
    rounds :: Int,
    state :: GameState
  }

defaultConfig :: C.Config
defaultConfig =
  C.Config
    { C.colors = 8,
      C.plength = 4
    }

nRounds :: Int
nRounds = 12

initialWorld :: StdGen -> World
initialWorld gen =
  World
    { game = G.newGame gen defaultConfig,
      curent = replicate (C.plength defaultConfig) 'a',
      past = [],
      rounds = nRounds,
      state = InProgress
    }

charColor :: Char -> Color
charColor 'a' = dark red
charColor 'b' = green
charColor 'c' = blue
charColor 'd' = orange
charColor 'e' = cyan
charColor 'f' = magenta
charColor 'g' = rose
charColor 'h' = violet
charColor _ = black

codeDot :: Char -> Picture
codeDot ch = Color (charColor ch) $ circleSolid (unit * 0.3)

answerDot :: Bool -> Picture
answerDot isRed = Color c $ circleSolid (unit * 0.15)
  where
    c = if isRed then red else yellow

drawCode :: Int -> Bool -> C.Code -> Picture
drawCode y legend code = Translate 0 (- unit * fromIntegral y) $ Pictures [codeDrawing, legendDrawing]
  where
    codeDrawing = Pictures $ zipWith drawCodeDot [0 ..] code
    drawCodeDot i = trans i . codeDot
    legendDrawing =
      if legend
        then Pictures (zipWith drawLegend [0 ..] ["Q", "W", "E", "R"])
        else Blank
    drawLegend i = Translate (-0.1 * unit) (-0.1 * unit) . trans i . Scale (unit * 0.002) (unit * 0.002) . Text
    trans i = Translate ((-1.5 * unit) + unit * fromIntegral i) 0

drawAnswer :: Int -> C.Answer -> Picture
drawAnswer y a@C.Answer {C.red = nred, C.yellow = nyellow} = Translate 0 (- unit * fromIntegral y) answerDrawing
  where
    answerDrawing = Pictures $ map drawAnswerDot [0 .. nred + nyellow -1]
    dotColor i = i < nred
    drawAnswerDot i = Translate ((-0.75 * unit) + 0.5 * unit * fromIntegral i) 0 . answerDot $ dotColor i

submitCode :: World -> World
submitCode world =
  world
    { game = G.roundsElapsedIncrease $ game world,
      past = (curent world, answer) : past world,
      state = s
    }
  where
    answer = C.evaluateGuess (G.password $ game world) (curent world)
    s
      | C.isWinningAnswer (G.config $ game world) answer = Won
      | length (past world) >= rounds world - 1 = Lost
      | otherwise = state world

drawWorld :: World -> Picture
drawWorld world = switchState (state world)
  where
    switchState s
      | s == Won = center . appendWinningCode . scaleText $ Text "WON"
      | s == Lost = center . appendWinningCode . scaleText $ Text "LOST"
      | otherwise = uncurry Translate originOffset $ Pictures [codesDrawing, answersDrawing]
    codesDrawing = Pictures $ zipWith3 drawCode [0 ..] (replicate (length $ past world) False ++ [True]) allCodes
    allCodes = reverse $ curent world : [c | (c, _) <- past world]
    answersDrawing = Translate (3.5 * unit) 0 $ Pictures $ zipWith drawAnswer [0 ..] allAnswers
    allAnswers = reverse [a | (_, a) <- past world]
    (wWidth, wHeight) = fromIntegral <$> wDimensions
    winningCodeDrawing = Translate (0.2 * fromIntegral wWidth) (- unit) $ drawCode 0 False (G.password $ game world)
    appendWinningCode = Pictures . (: [winningCodeDrawing])
    scaleText = Scale (0.01 * unit) (0.01 * unit)
    center = Translate (-0.2 * fromIntegral wWidth) 0

cycleDot :: Int -> World -> World
cycleDot i w = w {curent = newCurent}
  where
    newCurent = zipWith (\ch j -> if i == j then nextChar ch else ch) (curent w) [0 ..]
    nextChar ch = chr $ ord 'a' + (ord ch - ord 'a' + 1) `mod` C.colors (G.config $ game w)

handleKeys :: Event -> World -> World
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) = submitCode
handleKeys (EventKey (Char 'q') Down _ _) = cycleDot 0
handleKeys (EventKey (Char 'w') Down _ _) = cycleDot 1
handleKeys (EventKey (Char 'e') Down _ _) = cycleDot 2
handleKeys (EventKey (Char 'r') Down _ _) = cycleDot 3
handleKeys _ = id

unit :: Float
unit = 60

wDimensions :: (Int, Int)
wDimensions = (7 * round unit, (nRounds + 1) * round unit)

originOffset :: (Float, Float)
originOffset = (- 1.5 * unit, (wHeight - unit) / 2)
  where
    (wWidth, wHeight) = fromIntegral <$> wDimensions

run :: IO ()
run = do
  gen <- getStdGen
  play window backgroundColor updatesPerSecond (initialWorld gen) drawWorld handleKeys updateWorld
  where
    window = InWindow "logik" wDimensions (200, 200)
    backgroundColor = white
    updatesPerSecond = 20
    updateWorld = const id
