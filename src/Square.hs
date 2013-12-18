module Square (Square(Normal, DoubleLetter, DoubleWord, TripleWord), scoreWord) where

	import Tile
	import Data.List

	data Square = Normal (Maybe Tile)
				| DoubleLetter (Maybe Tile)
				| TripleLetter (Maybe Tile)
				| DoubleWord (Maybe Tile)
				| TripleWord (Maybe Tile) deriving Show

	{-
		Calculates the score of one played word.

		The first list contains squares that are already on the board
		(and thus are not subject to bonuses).

		The second list contains squares that are newly occupied.
	-}
	scoreWord :: [Square] -> [Square] -> Int
	-- Calculate the base score then add the letter bonuses (doubleLetter, doubleWord),
	-- then multiply by word bonuses (DoubleWord, TripleWord)
	scoreWord xs ys = addBonuses (addBonuses baseScore letterBonuses) wordBonuses
		where
			calcBaseScore squares = foldl (\acc square -> acc + baseValue square) (0) squares
			addBonuses score squares = foldl (\acc square -> applyWordBonus square acc) score squares			
			baseScore = (calcBaseScore xs) + (calcBaseScore ys)
			(wordBonuses, letterBonuses) = partition isWordBonus ys

	{-
		The bonus operations that should be applied to the score of a word
		once it has been calculated. When using to calculate score, 
		double and triple word bonuses should be applied last.

	-}
	applyWordBonus :: Square -> (Int -> Int)
	applyWordBonus (Normal (Just tile)) = (+0)
	applyWordBonus (DoubleLetter (Just tile)) = (+ (tileValue tile) )
	applyWordBonus (TripleLetter (Just tile)) = (+ ((tileValue tile) * 2))
	applyWordBonus (DoubleWord (Just tile)) = (*2)
	applyWordBonus (TripleWord (Just tile)) = (*3)
	applyWordBonus _ = (+0)

	isWordBonus :: Square -> Bool
	isWordBonus (DoubleWord _) = True
	isWordBonus (TripleWord _) = True
	isWordBonus _ = False

	-- Base value of a square, without bonuses
	baseValue :: Square -> Int
	baseValue (Normal (Just tile)) = tileValue tile
	baseValue (DoubleLetter (Just tile)) = tileValue tile
	baseValue (TripleLetter (Just tile)) = tileValue tile
	baseValue (DoubleWord (Just tile)) = tileValue tile
	baseValue (TripleWord (Just tile)) = tileValue tile
	baseValue _ = 0
