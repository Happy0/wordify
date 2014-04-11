module LetterBag.Internal (LetterBag(LetterBag), tiles, bagSize) where

	import Tile

	data LetterBag = LetterBag { tiles :: [Tile],  bagSize :: Int } deriving (Show, Eq)
