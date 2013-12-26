module FormedWord where

  import Pos
  import Square
  import Tile
  import Board
  import ScrabbleError
  import Data.Sequence
  import Data.Map as Map
  import Control.Applicative

  data FormedWords = FormedWords { formedWords :: [FormedWord], board :: Board, placed :: Map Pos Tile }
  data FormedWord = FormedWord (Seq (Pos, Square))
  data Direction = Horizontal | Vertical deriving Eq

  wordsFormed :: Board -> Map Pos Tile -> Either ScrabbleError FormedWords
  wordsFormed board tiles = Left $ NotEnoughLettersInStartingBag 2

    where


      preceding direction pos = case direction of
                                  Horizontal -> lettersLeft board pos
                                  Vertical -> lettersBelow board pos
      after direction pos =  case direction of
                                  Horizontal -> lettersRight board pos
                                  Vertical -> lettersBelow board pos

      (minPos, firstTile) = Map.findMin tiles
      (maxPos, lastTile) = Map.findMax tiles

      swapDirection direction = if direction == Horizontal then Vertical else Horizontal

      getDirection
        | (xPos minPos) == (xPos maxPos) = Just Horizontal
        | (yPos minPos) == (yPos maxPos) = Just Vertical
        | otherwise = Nothing 
