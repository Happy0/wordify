module Tests.SharedTestData where

import qualified Data.Map as M
import Data.Maybe
import Wordify.Rules.LetterBag
import Wordify.Rules.Pos
import Wordify.Rules.Pos.Internal
import Wordify.Rules.Square
import Wordify.Rules.Tile

horizontalPositions = catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (5, 7)

horizontalSquares = [Normal $ Just (Letter "H" 4), Normal $ Just (Letter "E" 1), DoubleLetter $ Just (Letter "L" 1), Normal $ Just (Letter "L" 1), DoubleLetter $ Just (Letter "O" 1)]

rogueLeft = (Pos 3 7 "C7", DoubleLetter $ Just (Letter "X" 2))

rogueRight = (Pos 11 7 "K7", Normal $ Just (Letter "Z" 2))

horizontals = zip horizontalPositions horizontalSquares

verticalPositions = catMaybes $ map posAt $ iterate (\(x, y) -> (x, y + 1)) (7, 5)

verticalSquares = [Normal $ Just (Letter "T" 1), Normal $ Just (Letter "E" 1), DoubleLetter $ Just (Letter "L" 1), Normal $ Just (Letter "L" 1), DoubleLetter $ Just (Letter "Y" 4)]

rogueAbove = (Pos 7 3 "G3", DoubleLetter $ Just (Letter "X" 2))

rogueBelow = (Pos 7 11 "G11", Normal $ Just (Letter "Z" 2))

verticals = zip verticalPositions verticalSquares

isValid :: Either a b -> Bool
isValid (Right _) = True
isValid _ = False