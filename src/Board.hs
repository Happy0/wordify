module Board(emptyBoard) where

  import Square
  import Pos
  import Data.Maybe
  import qualified Data.Map as Map

  data Board = Board (Map.Map Pos Square) deriving Show

  {-
    Creates an empty board. 
  -}
  emptyBoard :: Board
  emptyBoard = Board (Map.fromList $ posSquares)
    where
      layout = [  ["TW","N","N","DL","N","N","N","TW","N","N","N","DL","N","N","TW"]
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
      labeledSquares= concat $ map (\(x, xs) -> columnToMapping x xs) columns
      -- im sry
      columnToMapping columnNo columnSquares = zipWith (\sq y -> ((columnNo,y),sq)) columnSquares [1..15]
      posSquares = mapMaybe (\((x,y), sq) -> fmap (\pos -> (pos, sq)) (posAt (x,y))) labeledSquares

      toSquare :: String -> Square
      toSquare "N" = Normal Nothing
      toSquare "DL" = DoubleLetter Nothing
      toSquare "TL" = TripleLetter Nothing
      toSquare "DW" = DoubleWord Nothing
      toSquare "TW" = TripleWord Nothing 

