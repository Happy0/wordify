module Tests.BoardTest where

    import Board.Internal
    import Pos
    import Data.Maybe
    import Square
    import Tile
    import Square
    import qualified Data.Map as M
    import Pos.Internal
    import Test.HUnit.Base
    import Board
    import qualified Data.Sequence as Seq

    horizontalPositions = catMaybes $ map posAt $ iterate (\(x,y) -> (x + 1, y)) (5,7)
    horizontalSquares = [Normal $ Just (Letter 'H' 2), Normal $ Just (Letter 'E' 3), DoubleLetter $ Just (Letter 'L' 2), Normal $ Just (Letter 'L' 3), DoubleLetter $ Just (Letter 'O' 4)]
    rogueLeft = (Pos 3 7 "C7", DoubleLetter $ Just (Letter 'X' 2))
    rogueRight = (Pos 11 7 "K7", DoubleLetter $ Just (Letter 'Z' 2))
    horizontals = zip horizontalPositions horizontalSquares

    verticalPositions = catMaybes $ map posAt $ iterate (\(x,y) -> (x, y + 1)) (7,5)
    verticalSquares = [Normal $ Just (Letter 'T' 2), Normal $ Just (Letter 'E' 3), DoubleLetter $ Just (Letter 'L' 2), Normal $ Just (Letter 'L' 3), DoubleLetter $ Just (Letter 'Y' 4)]
    rogueAbove = (Pos 7 3 "G3", DoubleLetter $ Just (Letter 'X' 2))
    rogueBelow = (Pos 7 11 "G11", DoubleLetter $ Just (Letter 'Z' 2))
    verticals = zip verticalPositions verticalSquares

    testBoard :: Board
    testBoard = Board squareMap
        where
            squareMap = M.fromList $ horizontals ++ verticals ++ [rogueLeft] ++ [rogueRight] ++ [rogueAbove] ++ [rogueBelow]

    lettersLeftTest :: Assertion
    lettersLeftTest = 
        do
            let pos = Pos 9 7 "I7"
            let actual = lettersLeft testBoard pos
            let expected = Seq.fromList $ init horizontals

            assertEqual "Unexpected result for letters left" expected actual

    lettersRightTest :: Assertion
    lettersRightTest =
        do
            let pos = Pos 5 7 "E7"
            let actual = lettersRight testBoard pos
            let expected = Seq.fromList $ tail horizontals

            assertEqual "Unexpected result for letters left" expected actual

    lettersAboveTest :: Assertion
    lettersAboveTest = 
        do
            let pos = Pos 7 5 "G5"
            let actual = lettersAbove testBoard pos
            let expected = Seq.fromList $ tail verticals

            assertEqual "Unexpected result for letters above" expected actual

    lettersBelowTest :: Assertion
    lettersBelowTest = 
        do
            let pos = Pos 7 9 "G9"
            let actual = lettersBelow testBoard pos
            let expected = Seq.fromList $ init verticals

            assertEqual "Unexpected result for letters below" expected actual




