module Tile (Tile(Letter, Blank), tileValue, isPlayable, tileLetter) where

{-
A tile is a letter with a value, or a Blank tile
which may have been given a letter. Blank tiles
always have the value '0'.
-}
data Tile = Letter Char Int | Blank (Maybe Char) deriving (Show, Eq, Ord)

tileValue :: Tile -> Int
tileValue (Letter _ val) = val
tileValue (Blank _) = 0

tileLetter :: Tile -> Maybe Char
tileLetter (Letter char _) = Just char
tileLetter (Blank (Just char)) = Just char
tileLetter (Blank Nothing) = Nothing

{-
  isPlayble, applied to a played tile and compared against a tile
  returns true if a player returned a letter tile on their rack,
  or if the player played a Blank that has been given a letter
-}
isPlayable :: Tile -> Tile -> Bool
isPlayable (Letter a b) (Letter x y) = (a == x) && (b == y)
isPlayable (Blank (Just _)) (Blank Nothing) = True
isPlayable _ _ = False