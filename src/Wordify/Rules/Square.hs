module Wordify.Rules.Square (Square(Normal, DoubleLetter, TripleLetter, DoubleWord, TripleWord),
     isOccupied, scoreWord, squareIfOccupied, putTileOn, tileIfOccupied) where

  import Wordify.Rules.Tile
  import Data.Sequence as Seq
  import qualified Data.Foldable as F
  import Data.Maybe

  data Square = Normal (Maybe Tile)
        | DoubleLetter (Maybe Tile)
        | TripleLetter (Maybe Tile)
        | DoubleWord (Maybe Tile)
        | TripleWord (Maybe Tile) deriving (Show,Eq,Ord)

  putTileOn :: Square -> Tile -> Square
  putTileOn (Normal _) tile = Normal $ Just tile
  putTileOn (DoubleLetter _) tile = DoubleLetter $ Just tile
  putTileOn (TripleLetter _) tile = TripleLetter $ Just tile
  putTileOn (DoubleWord _) tile = DoubleWord $ Just tile
  putTileOn (TripleWord _) tile = TripleWord $ Just tile

  {- |
    Calculates the score of one played word.

    The first list contains squares that are already on the board
    (and thus are not subject to bonuses).

    The second list contains squares that are newly occupied.
  -}
  scoreWord :: Seq Square -> Seq Square -> Int
  -- Calculate the base score then add the letter bonuses (doubleLetter, doubleWord),
  -- then multiply by word bonuses (DoubleWord, TripleWord)
  scoreWord xs ys = addBonuses (addBonuses baseScore letterBonuses) wordBonuses
    where
      calcBaseScore = F.foldr (\square acc -> baseValue square + acc) 0
      addBonuses = F.foldr applyWordBonus
      baseScore = calcBaseScore xs + calcBaseScore ys
      (wordBonuses, letterBonuses) = Seq.partition isWordBonus ys

  {- |
    The bonus operations that should be applied to the score of a word
    once it has been calculated. When using to calculate score, 
    double and triple word bonuses should be applied last.

  -}
  applyWordBonus :: Square -> Int -> Int
  applyWordBonus (Normal _ ) = (+0)
  applyWordBonus (DoubleLetter (Just tile)) = (+ tileValue tile)
  applyWordBonus (TripleLetter (Just tile)) = (+ tileValue tile * 2)
  applyWordBonus (DoubleWord _) = (*2)
  applyWordBonus (TripleWord _) = (*3)
  applyWordBonus _ = (+0)

  isWordBonus :: Square -> Bool
  isWordBonus (DoubleWord _) = True
  isWordBonus (TripleWord _) = True
  isWordBonus _ = False

  -- | Base value of a square, without bonuses
  baseValue :: Square -> Int
  baseValue sq = maybe 0 tileValue $ tileIfOccupied sq

  squareIfOccupied :: Square -> Maybe Square
  squareIfOccupied sq = tileIfOccupied sq >> Just sq

  tileIfOccupied :: Square -> Maybe Tile
  tileIfOccupied (Normal (Just tile)) = Just tile
  tileIfOccupied (DoubleLetter (Just tile)) = Just tile
  tileIfOccupied (TripleLetter (Just tile)) = Just tile
  tileIfOccupied (DoubleWord (Just tile)) = Just tile
  tileIfOccupied (TripleWord (Just tile)) = Just tile
  tileIfOccupied _ = Nothing

  isOccupied :: Square -> Bool
  isOccupied = isJust . squareIfOccupied
