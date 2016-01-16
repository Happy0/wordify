module Wordify.Rules.LetterBag.Internal (LetterBag(LetterBag), tiles, bagSize, validLetters, generator) where

    import Wordify.Rules.Tile
    import System.Random
    import Data.Map

    data LetterBag = LetterBag { tiles :: [Tile],  bagSize :: Int, generator :: StdGen, validLetters :: Map Char Tile } deriving (Show)

    instance Eq LetterBag where
        bag1 == bag2 = (bagSize bag1 == bagSize bag2 && tiles bag1 == tiles bag2)
