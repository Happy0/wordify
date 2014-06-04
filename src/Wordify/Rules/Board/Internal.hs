module Wordify.Rules.Board.Internal (Board(Board)) where

    import Wordify.Rules.Pos
    import Wordify.Rules.Square
    import qualified Data.Map as Map

    data Board = Board (Map.Map Pos Square) deriving (Show, Eq)
