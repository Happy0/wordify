module Wordify.Rules.Board
  ( Board,
    emptyBoard,
    allSquares,
    placeTile,
    placeTiles,
    occupiedSquareAt,
    emptySquaresFrom,
    lettersAbove,
    lettersBelow,
    lettersLeft,
    lettersRight,
    unoccupiedSquareAt,
    textRepresentation,
    loadFromTextRepresentation,
    prettyPrint,
  )
where

import Control.Applicative
import Control.Monad
import Data.Foldable as F
import qualified Data.List as L
import Data.List.Split as S
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence as Seq
import qualified Data.Text as T
import Wordify.Rules.Board.Internal
import Wordify.Rules.Pos
import Wordify.Rules.Square
import Wordify.Rules.Tile

instance Show Board where
  show = prettyPrint

-- |
--    Returns all the squares on the board, ordered by column then row.
allSquares :: Board -> [(Pos, Square)]
allSquares (Board squares) = Map.toList squares

-- |
--    Places a tile on a square and yields the new board, if the
--    target square is empty. Otherwise yields 'Nothing'.
placeTile :: Board -> Tile -> Pos -> Maybe Board
placeTile board tile pos =
  (\sq -> insertSquare board pos (putTileOn sq tile)) <$> unoccupiedSquareAt board pos

-- |
--    Places tiles on the given squares and yields the new board, if
--    all the target squares are empty. Otherwise yields 'Nothing'.
placeTiles :: Board -> [(Tile, Pos)] -> Maybe Board
placeTiles board = foldl tryPlaceTile (Just board)
  where
    tryPlaceTile :: Maybe Board -> (Tile, Pos) -> Maybe Board
    tryPlaceTile Nothing (tile, pos) = Nothing
    tryPlaceTile (Just board) (tile, pos) = placeTile board tile pos

insertSquare :: Board -> Pos -> Square -> Board
insertSquare (Board squares) pos square = Board $ Map.insert pos square squares

squareAt :: Board -> Pos -> Maybe Square
squareAt (Board squares) = flip Map.lookup squares

-- | Returns the square at a given position if it is not occupied by a tile. Otherwise returns Nothing.
unoccupiedSquareAt :: Board -> Pos -> Maybe Square
unoccupiedSquareAt board pos =
  squareAt board pos >>= (\sq -> if isOccupied sq then Nothing else Just sq)

-- | Returns the square at a given position if it is occupied by a tile. Otherwise returns Nothing.
occupiedSquareAt :: Board -> Pos -> Maybe Square
occupiedSquareAt board pos = squareAt board pos >>= squareIfOccupied

-- | All letters immediately above a given square until a non-occupied square
lettersAbove :: Board -> Pos -> Seq (Pos, Square)
lettersAbove board pos = walkFrom board pos above

-- | All letters immediately below a given square until a non-occupied square
lettersBelow :: Board -> Pos -> Seq (Pos, Square)
lettersBelow board pos = Seq.reverse $ walkFrom board pos below

-- | All letters immediately left of a given square until a non-occupied square
lettersLeft :: Board -> Pos -> Seq (Pos, Square)
lettersLeft board pos = Seq.reverse $ walkFrom board pos left

-- | All letters immediately right of a given square until a non-occupied square
lettersRight :: Board -> Pos -> Seq (Pos, Square)
lettersRight board pos = walkFrom board pos right

-- | Finds the empty square positions horizontally or vertically from a given position,
--       skipping any squares that are occupied by a tile
emptySquaresFrom :: Board -> Pos -> Int -> Direction -> [Pos]
emptySquaresFrom board startPos numSquares direction =
  let changingOnwards = [changing .. 15]
   in L.take numSquares $
        L.filter (isJust . unoccupiedSquareAt board) $
          mapMaybe posAt $ zipDirection (repeat constant) changingOnwards
  where
    (constant, changing) = if direction == Horizontal then (yPos startPos, xPos startPos) else (xPos startPos, yPos startPos)
    zipDirection = if (direction == Horizontal) then flip L.zip else L.zip

-- | Pretty prints a board to a human readable string representation. Helpful for development.
prettyPrint :: Board -> String
prettyPrint board = rowsWithLabels ++ columnLabelSeparator ++ columnLabels
  where
    rows = L.transpose . S.chunksOf 15 . map (squareToString . snd) . allSquares
    rowsWithLabels = concatMap (\(rowNo, row) -> (rowStr rowNo) ++ concat row ++ "\n") . Prelude.zip [1 ..] $ (rows board)

    rowStr :: Int -> String
    rowStr number = if number < 10 then ((show number) ++ " | ") else (show number) ++ "| "
    columnLabelSeparator = "  " ++ (Prelude.take (15 * 5) $ repeat '-') ++ "\n"
    columnLabels = "      " ++ (concat $ Prelude.take (15 * 2) . L.intersperse "    " . map (: []) $ ['A' ..])

    squareToString square =
      case (tileIfOccupied square) of
        Just sq -> maybe " |_| " (\lt -> " |" ++ lt : "| ") $ tileLetter sq
        Nothing ->
          case square of
            (Normal _) -> "  N  "
            (DoubleLetter _) -> "  DL "
            (TripleLetter _) -> "  TL "
            (DoubleWord _) -> "  DW "
            (TripleWord _) -> "  TW "

{-
  Walks the tiles from a given position in a given direction
  until an empty square is found or the boundary of the board
  is reached.
-}
walkFrom :: Board -> Pos -> (Pos -> Maybe Pos) -> Seq (Pos, Square)
walkFrom board pos direction =
  maybe
    mzero
    ( \(next, sq) ->
        (next, sq) <| walkFrom board next direction
    )
    neighbourPos
  where
    neighbourPos =
      direction pos >>= \nextPos ->
        occupiedSquareAt board nextPos
          >>= \sq -> return (nextPos, sq)

{-
  Represents the board as a comma delimited string where each character is either the character in the board square
  or an empty string. The string is ordered by column then row, starting at position A1 and ending at O15.

  E.g. an empty board would be representated as 244 contiguous , characters. A

  A board with the tile 'H' at position H8 and 'I' at position H9 would look like this:
  ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,H,I,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
-}
textRepresentation :: Board -> String
textRepresentation board = L.intercalate "," (squareStrings board)
  where
    squareStrings :: Board -> [String]
    squareStrings board = map getLetterRepresentation (allSquares board)

    getLetterRepresentation :: (Pos, Square) -> String
    getLetterRepresentation square = toLetterRepresentation ((tileIfOccupied . snd) square >>= tileLetter)

    toLetterRepresentation :: Maybe Char -> String
    toLetterRepresentation (Just char) = [char]
    toLetterRepresentation Nothing = ""

{-
  Loads a board from the string representation of the board generated by the 'textPresentation' function
-}
loadFromTextRepresentation :: M.Map Char Tile -> String -> Maybe Board
loadFromTextRepresentation validTiles textRepresentation =
  let positionsWithLetters = L.zip [1 ..] textRepresentation
   in let placements = mapMaybe (uncurry positionWithLetter) positionsWithLetters
       in placeTiles emptyBoard placements
  where
    positionWithLetter :: Int -> Char -> Maybe (Tile, Pos)
    positionWithLetter oneDimensionalCoordinate letter = do
      tile <- M.lookup letter validTiles
      let x = (oneDimensionalCoordinate `div` 15) + 1
      let y = (oneDimensionalCoordinate `mod` 15) + 1
      coordinate <- posAt (x, y)
      return (tile, coordinate)

-- |
--    Creates an empty board.
emptyBoard :: Board
emptyBoard = Board (Map.fromList posSquares)
  where
    layout =
      [ ["TW", "N", "N", "DL", "N", "N", "N", "TW", "N", "N", "N", "DL", "N", "N", "TW"],
        ["N", "DW", "N", "N", "N", "TL", "N", "N", "N", "TL", "N", "N", "N", "DW", "N"],
        ["N", "N", "DW", "N", "N", "N", "DL", "N", "DL", "N", "N", "N", "DW", "N", "N"],
        ["DL", "N", "N", "DW", "N", "N", "N", "DL", "N", "N", "N", "DW", "N", "N", "DL"],
        ["N", "N", "N", "N", "DW", "N", "N", "N", "N", "N", "DW", "N", "N", "N", "N"],
        ["N", "TL", "N", "N", "N", "TL", "N", "N", "N", "TL", "N", "N", "N", "TL", "N"],
        ["N", "N", "DL", "N", "N", "N", "DL", "N", "DL", "N", "N", "N", "DL", "N", "N"],
        ["TW", "N", "N", "DL", "N", "N", "N", "DW", "N", "N", "N", "DL", "N", "N", "TW"],
        ["N", "N", "DL", "N", "N", "N", "DL", "N", "DL", "N", "N", "N", "DL", "N", "N"],
        ["N", "TL", "N", "N", "N", "TL", "N", "N", "N", "TL", "N", "N", "N", "TL", "N"],
        ["N", "N", "N", "N", "DW", "N", "N", "N", "N", "N", "DW", "N", "N", "N", "N"],
        ["DL", "N", "N", "DW", "N", "N", "N", "DL", "N", "N", "N", "DW", "N", "N", "DL"],
        ["N", "N", "DW", "N", "N", "N", "DL", "N", "DL", "N", "N", "N", "DW", "N", "N"],
        ["N", "DW", "N", "N", "N", "TL", "N", "N", "N", "TL", "N", "N", "N", "DW", "N"],
        ["TW", "N", "N", "DL", "N", "N", "N", "TW", "N", "N", "N", "DL", "N", "N", "TW"]
      ]

    squares = (map . map) toSquare layout
    columns = Prelude.zip [1 .. 15] squares
    labeledSquares = concatMap (uncurry columnToMapping) columns
    columnToMapping columnNo columnSquares = Prelude.zipWith (\sq y -> ((columnNo, y), sq)) columnSquares [1 .. 15]
    posSquares = mapMaybe (\((x, y), sq) -> fmap (\pos -> (pos, sq)) (posAt (x, y))) labeledSquares

    toSquare :: String -> Square
    toSquare "DL" = DoubleLetter Nothing
    toSquare "TL" = TripleLetter Nothing
    toSquare "DW" = DoubleWord Nothing
    toSquare "TW" = TripleWord Nothing
    toSquare _ = Normal Nothing
