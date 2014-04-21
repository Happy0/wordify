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
    import Control.Monad
    import qualified Data.Set as S
    import Tests.SharedTestData

    allTiles = horizontals ++ verticals ++ [rogueLeft] ++ [rogueRight] ++ [rogueAbove] ++ [rogueBelow]

    {- Verifies that tiles can only be placed on squares which are empty -}
    placeTileProperty :: Board -> Pos -> Tile -> Bool
    placeTileProperty board pos tile =
     if (isNothing targetSquare) then placeResult == Just (Board $ M.insert pos newSquare squareMap) else isNothing placeResult
        where
            placeResult = placeTile board tile pos
            Board (squareMap) = board
            targetSquare = occupiedSquareAt board pos
            newSquare = putTileOn (fromJust (unoccupiedSquareAt board pos)) tile

    testBoard :: Board
    testBoard = Board squareMap
        where
            squareMap = M.fromList $ (M.assocs emptySquares ++ allTiles)
            Board (emptySquares) = emptyBoard

    squareAtTest :: Assertion
    squareAtTest = 
        do
            let pos = Pos 3 7 "C7"
            let expected = Just $ DoubleLetter $ Just (Letter 'X' 2)
            let actual = squareAt testBoard pos

            assertEqual "Unexpected result for squareAt function" expected actual

    occupiedSquareAtTest :: Assertion
    occupiedSquareAtTest =
        do
            let pos = Pos 3 7 "C7"
            let expected = Just $ DoubleLetter $ Just (Letter 'X' 2)
            let actual = occupiedSquareAt testBoard pos

            assertEqual "Unexpected result for occupiedSquareAt function where square is occupied" expected actual

    occupiedSquareAtUnoccupiedTest :: Assertion
    occupiedSquareAtUnoccupiedTest =
        do
            let unoccupiedPos = Pos 1 1 "A1"
            let expected = Nothing
            let actual = occupiedSquareAt testBoard unoccupiedPos

            assertEqual "Unexpected result for occupiedSquareAt function where square is unoccupied" expected actual

    unoccupiedSquareAtTest :: Assertion
    unoccupiedSquareAtTest = 
        do
            let unoccupiedPos = Pos 1 1 "A1"
            let expected = Just $ TripleWord Nothing
            let actual = unoccupiedSquareAt testBoard unoccupiedPos

            assertEqual "Unexpected result for unoccupiedSquareAt function where square is unoccupied" expected actual

    unoccupiedSquareAtTestOccupied :: Assertion
    unoccupiedSquareAtTestOccupied =
        do
            let occupiedPos = Pos 3 7 "C7"
            let expected = Nothing
            let actual = unoccupiedSquareAt testBoard occupiedPos

            assertEqual "Unexpected result for unoccupiedSquareAt function where square is unoccupied" expected actual

    allSquaresTest :: Assertion
    allSquaresTest = 
        do
            let Board (squareMap) = testBoard
            let expected = M.toList squareMap
            let actual = allSquares testBoard

            assertEqual "Unexpected result for allSquares function" expected actual

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

    {- Verifies that new tiles can be placed consecutively on a board while the old board is retained -}
    tilesPlacedConsecutivelyTest :: Assertion
    tilesPlacedConsecutivelyTest =
        do
            let expected = testBoard
            let tiles = map (\(pos, square) -> (pos, fromJust $ tileIfOccupied square) ) $ (S.toList . S.fromList) allTiles
            let result = foldM (\board (tile, pos) -> placeTile board pos tile) emptyBoard tiles
            maybe (assertFailure "Tiles placed test failed") (\actual -> assertEqual "Tiles were not placed on board" expected actual) result

            

