module Board(emptyBoard, occupiedSquareAt, squareIsOccupied,
 lettersAbove, lettersBelow, lettersLeft, lettersRight) where

  import Square
  import Pos
  import Data.Maybe
  import qualified Data.Map as Map
  import Control.Monad

  data Board = Board (Map.Map Pos Square) deriving Show

  occupiedSquareAt :: Board -> Pos -> Maybe Square
  occupiedSquareAt (Board positions) pos = Map.lookup pos positions >>= squareIfOccupied

  squareIsOccupied :: Board -> Pos -> Bool
  squareIsOccupied board pos = isJust $ occupiedSquareAt board pos

  -- AddTile ...
 
  lettersAbove :: Board -> Pos -> [(Pos,Square)]
  lettersAbove board pos = walkFrom board pos above

  lettersBelow :: Board -> Pos -> [(Pos,Square)]
  lettersBelow board pos = reverse $ walkFrom board pos below

  lettersLeft :: Board -> Pos -> [(Pos,Square)]
  lettersLeft board pos = reverse $ walkFrom board pos left

  lettersRight :: Board -> Pos -> [(Pos,Square)]
  lettersRight board pos = walkFrom board pos right

  {-
    Walks the tiles from a given position in a given direction
    until an empty square is found or the boundary of the board
    is reached.
  -}
  walkFrom :: Board -> Pos -> (Pos -> Maybe Pos) -> [(Pos,Square)]
  walkFrom board pos direction = maybe (mzero) (\(next,sq) ->
   (next, sq) : (walkFrom board next direction) ) nextPos
    where
      nextPos :: Maybe ((Pos, Square))
      nextPos = direction(pos) >>= (\nextPos -> occupiedSquareAt board nextPos >>=
        (\sq -> return (nextPos, sq) ))

  {-
    Creates an empty board. 
  -}
  emptyBoard :: Board
  emptyBoard = Board (Map.fromList posSquares)
    where
      layout = [["TW","N","N","DL","N","N","N","TW","N","N","N","DL","N","N","TW"]
       ,["N","DW","N","N","N","TL","N","N","N","TL","N","N","N","DW","N"]
       ,["N","N","DW","N","N","N","DL","N","DL","N","N","N","DW","N","N"]
       ,["DL","N","N","DW","N","N","N","DL","N","N","N","DW","N","N","DL"]
       ,["N","N","N","N","DW","N","N","N","N","N","DW","N","N","N","N"]
       ,["N","TL","N","N","N","TL","N","N","N","TL","N","N","N","TL","N"]
       ,["N","N","DL","N","N","N","DL","N","DL","N","N","N","DL","N","N"]
       ,["TW","N","N","DL","N","N","N","DW","N","N","N","DL","N","N","TW"]
       ,["N","N","DL","N","N","N","DL","N","DL","N","N","N","DL","N","N"]
       ,["N","TL","N","N","N","TL","N","N","N","TL","N","N","N","TL","N"]
       ,["N","N","N","N","DW","N","N","N","N","N","DW","N","N","N","N"]
       ,["DL","N","N","DW","N","N","N","DL","N","N","N","DW","N","N","DL"]
       ,["N","N","DW","N","N","N","DL","N","DL","N","N","N","DW","N","N"]
       ,["N","DW","N","N","N","TL","N","N","N","TL","N","N","N","DW","N"]
       ,["TW","N","N","DL","N","N","N","TW","N","N","N","DL","N","N","TW"]]

      squares = (map . map) toSquare layout
      columns = zip [1..15] squares
      -- pls
      labeledSquares= concatMap (\(x, xs) -> columnToMapping x xs) columns
      -- im sry
      columnToMapping columnNo columnSquares = zipWith (\sq y -> ((columnNo,y),sq)) columnSquares [1..15]
      -- ornicar pls
      posSquares = mapMaybe (\((x,y), sq) -> fmap (\pos -> (pos, sq)) (posAt (x,y))) labeledSquares

      toSquare :: String -> Square
      toSquare "N" = Normal Nothing
      toSquare "DL" = DoubleLetter Nothing
      toSquare "TL" = TripleLetter Nothing
      toSquare "DW" = DoubleWord Nothing
      toSquare "TW" = TripleWord Nothing 

