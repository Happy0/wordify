module LetterBag.Internal (LetterBag(LetterBag), tiles, bagSize, generator) where

    import Tile
    import System.Random

    data LetterBag = LetterBag { tiles :: [Tile],  bagSize :: Int, generator :: StdGen } deriving (Show)

    instance Eq LetterBag where
        bag1 == bag2 = (bagSize bag1 == bagSize bag2 && tiles bag1 == tiles bag2)
