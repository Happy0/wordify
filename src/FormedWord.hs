module FormedWord where

  import Pos
  import Square
  import Tile
  import Board
  import ScrabbleError
  import Data.Sequence as Seq
  import Data.Map as Map
  import Control.Applicative
  import Control.Monad
  import Data.Foldable

  data FormedWords = FormedWords { mainWord :: FormedWord, otherWords :: [FormedWord], board :: Board, placed :: Map Pos Square }
  type FormedWord = Seq (Pos, Square)
  data Direction = Horizontal | Vertical deriving Eq

  wordsFormed :: Board -> Map Pos Square -> Either ScrabbleError FormedWords
  wordsFormed board tiles = formedWords >>= (\words -> 
    case words of
      x : xs -> Right $ FormedWords x xs board tiles
      x : [] -> Right $ FormedWords x [] board tiles
      )

    where

      formedWords = maybe (Left $ MisplacedLetter maxPos lastTile) (\direction -> 
          middleFirstWord direction >>= (\middleFirstWord -> 
                          let (midWord, square) = middleFirstWord
                          in Right $ (preceding direction minPos >< midWord >< after direction maxPos) : adjacentWords (swapDirection direction) ) ) getDirection

      preceding direction pos = case direction of
                                  Horizontal -> lettersLeft board pos
                                  Vertical -> lettersBelow board pos
      after direction pos =  case direction of
                                  Horizontal -> lettersRight board pos
                                  Vertical -> lettersAbove board pos

      (minPos, firstTile) = Map.findMin tiles
      (maxPos, lastTile) = Map.findMax tiles

      adjacentWords direction = Prelude.map (\(pos, square) -> (preceding direction pos |> (pos, square)) >< after direction pos) placedList

      middleFirstWord direction = foldM (\(word, lastPos) (pos, square) -> 
          if (not $ stillOnPath lastPos pos direction) then Left $ MisplacedLetter pos square
            else 
              if (isDirectlyAfter pos lastPos direction) then Right $ (( word |> (pos, square)), pos) else
                let between = after direction lastPos in
                if expectedLettersInbetween direction lastPos pos between then Right $ ( word ><  ( between |> (pos,square) ), pos)
                  else Left $ MisplacedLetter pos square


        ) ( Seq.empty, minPos ) $ placedList

      placedList = Map.toList tiles

      swapDirection direction = if direction == Horizontal then Vertical else Horizontal

      stillOnPath lastPos thisPos direction = (directionGetter direction thisPos) == directionGetter direction lastPos
      expectedLettersInbetween direction lastPos currentPos squares = directionGetter direction currentPos - directionGetter direction lastPos == 0 

      directionGetter direction pos = if direction == Horizontal then xPos pos else yPos pos
      isDirectlyAfter pos nextPos direction = (directionGetter direction nextPos) == (directionGetter direction pos) + 1

      getDirection
        | (xPos minPos) == (xPos maxPos) = Just Horizontal
        | (yPos minPos) == (yPos maxPos) = Just Vertical
        | otherwise = Nothing