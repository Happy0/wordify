module Board.Internal (Board(Board)) where

    import Pos
    import Square
    import qualified Data.Map as Map

    data Board = Board (Map.Map Pos Square) deriving Show
