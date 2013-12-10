module Tile (Tile, tileValue) where

{-
A tile is a letter with a value, or a Blank tile
which may have been given a letter. Blank tiles
always have the value '0'.
-}
data Tile = Letter Char Int | Blank (Maybe Char)

tileValue :: Tile -> Int
tileValue (Letter _ val) = val
tileValue (Blank _) = 0

