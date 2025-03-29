module Wordify.Rules.Tile (Tile (Letter, Blank), tileValue, isPlayable, tileString, printString) where

import Data.Char

-- |
-- A tile is a string with a value, or a Blank tile
-- which may have been given a letter. Blank tiles
-- always have the value '0'.
data Tile = Letter String Int | Blank (Maybe String) deriving (Show, Eq, Ord)

tileValue :: Tile -> Int
tileValue (Letter _ val) = val
tileValue (Blank _) = 0

tileString :: Tile -> Maybe String
tileString (Letter string _) = Just string
tileString (Blank (Just string)) = Just string
tileString (Blank Nothing) = Nothing

-- |
-- 	Prints a letter in the style found on a scoresheet. E.g. blank letters are printed in lowercase.
printString :: Tile -> Maybe String
printString (Letter string _) = Just string
printString (Blank (Just string)) = Just $ map toLower string
printString _ = Nothing

-- |
--  isPlayble, applied to a played tile and compared against a tile
--  returns true if a player returned a letter tile on their rack,
--  or if the player played a Blank that has been given a letter
isPlayable :: Tile -> Tile -> Bool
isPlayable (Letter a b) (Letter x y) = (a == x) && (b == y)
isPlayable (Blank (Just _)) (Blank Nothing) = True
isPlayable _ _ = False