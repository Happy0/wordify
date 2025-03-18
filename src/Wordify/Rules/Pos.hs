module Wordify.Rules.Pos
  ( Pos,
    Direction (Horizontal, Vertical),
    posAt,
    above,
    below,
    left,
    right,
    abovePositions,
    belowPositions,
    leftPositions,
    rightPositions,
    xPos,
    yPos,
    gridValue,
    direction,
    starPos,
    posMin,
    posMax,
  )
where

import qualified Data.Map as Map
import Data.Maybe
import Wordify.Rules.Pos.Internal

data Direction = Horizontal | Vertical deriving (Eq)

posMin :: Int
posMin = 1

posMax :: Int
posMax = 15

posAt :: (Int, Int) -> Maybe Pos
posAt = flip Map.lookup posMap

-- | The position above the given position, if it exists.
above :: Pos -> Maybe Pos
above (Pos x y _) = posAt (x, y + 1)

-- | The position below the given position, if it exists.
below :: Pos -> Maybe Pos
below (Pos x y _) = posAt (x, y - 1)

-- | The position to the left of the given position, if it exists.
left :: Pos -> Maybe Pos
left (Pos x y _) = posAt (x - 1, y)

-- | The position to the right of the given position, if it exists.
right :: Pos -> Maybe Pos
right (Pos x y _) = posAt (x + 1, y)

-- | The positions above the position (inclusive)
abovePositions :: Pos -> Int -> [Pos]
abovePositions pos = walkTimes pos above

-- | The positions below the position (inclusive)
belowPositions :: Pos -> Int -> [Pos]
belowPositions pos = walkTimes pos below

-- | The positions to the right of the position (inclusive)
rightPositions :: Pos -> Int -> [Pos]
rightPositions pos = walkTimes pos right

-- | The positions to the left of the position (inclusive)
leftPositions :: Pos -> Int -> [Pos]
leftPositions pos = walkTimes pos left

walkTimes :: Pos -> (Pos -> Maybe Pos) -> Int -> [Pos]
walkTimes startPosition travelFunction = flip take (walkDirection startPosition travelFunction)

walkDirection :: Pos -> (Pos -> Maybe Pos) -> [Pos]
walkDirection startPosition travelFunction =
  case travelFunction startPosition of
    Nothing -> [startPosition]
    Just nextPosition -> startPosition : walkDirection nextPosition travelFunction

-- | The position of the star square
starPos :: Pos
starPos = Pos 8 8 "H8"

direction :: Pos -> Pos -> Maybe Direction
direction startPos endPos
  | xPos startPos == xPos endPos = Just Vertical
  | yPos startPos == yPos endPos = Just Horizontal
  | otherwise = Nothing

{- A map keyed by tuples representing (x,y) co-ordinates, and valued by their
corresponding Pos types -}
posMap :: Map.Map (Int, Int) Pos
posMap = Map.fromList $ catMaybes coordTuples
  where
    coordTuples = zipWith makeTuple (sequence [[posMin .. posMax], [posMin .. posMax]]) $ cycle ['A' .. 'O']
    makeTuple (x : y : _) gridLetter = Just $ ((y, x), Pos y x (gridLetter : show x))
    makeTuple _ _ = Nothing

xPos :: Pos -> Int
xPos (Pos x _ _) = x

yPos :: Pos -> Int
yPos (Pos _ y _) = y

gridValue :: Pos -> String
gridValue (Pos _ _ grid) = grid
