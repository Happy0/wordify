module Tests.BoardTest where

    import Wordify.Rules.Board.Internal
    import Wordify.Rules.Pos
    import Data.Maybe
    import Wordify.Rules.Square
    import Wordify.Rules.Tile
    import Wordify.Rules.Square
    import qualified Data.Map as M
    import Wordify.Rules.Pos.Internal
    import Test.HUnit.Base
    import Wordify.Rules.Board
    import qualified Data.Sequence as Seq
    import Control.Monad
    import qualified Data.Set as S
    import Tests.SharedTestData
    import Data.List

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


    boardCorrectlyFormed :: Assertion
    boardCorrectlyFormed = 
        do
            let expectedTripleWords = catMaybes $ map (posAt) [(1,1), (8,1), (15,1), (1,8),(15,8), (1,15), (8,15),(15,15)]
            mapM_ (\pos -> assertEqual "Triple word squares not where expected" (Just (TripleWord Nothing)) (unoccupiedSquareAt emptyBoard pos)) expectedTripleWords

            let expectedDoubleWords = catMaybes $ map (posAt) [(2,2),(3,3),(4,4),(5,5), (8,8), (14,2), (13,3),(12,4),(11,5),(11,5),(4,12),(3,13),(2,14),(5,11),(11,11),(12,12),(13,13),(14,14)]
            mapM_ (\pos -> assertEqual "Double word squares not where expected" (Just (DoubleWord Nothing)) (unoccupiedSquareAt emptyBoard pos)) expectedDoubleWords

            let expectedTripleLetters = catMaybes $ map (posAt) [(6,2), (10,2),(2,6), (6,6),(10,6),(14,6),(2,10),(6,10),(10,10),(14,10),(6,14),(10,14)]
            mapM_ (\pos -> assertEqual "Triple letters squares not where expected" (Just (TripleLetter Nothing)) (unoccupiedSquareAt emptyBoard pos)) expectedTripleLetters

            let expectedDoubleLetters = catMaybes $ map (posAt) [(4,1),(12,1),(7,3),(9,3),(1,4),(8,4),(15,4),(3,7),(7,7),(9,7),(13,7),(4,8),(12,8),(3,9),(7,9),(9,9),(13,9),(1,12),(8,12),(15,12),(7,13),(9,13),(4,15),(12,15)]
            mapM_ (\pos -> assertEqual "Double letter squares not where expected" (Just (DoubleLetter Nothing)) (unoccupiedSquareAt emptyBoard pos)) expectedDoubleLetters

            let allPositions = catMaybes $ map posAt $ map (\[x,y] -> (x,y)) (sequence [[posMin..posMax], [posMin..posMax]])

            let expectedNormals = allPositions \\ (expectedTripleWords ++ expectedDoubleWords ++ expectedTripleLetters ++ expectedDoubleLetters)
            mapM_ (\pos -> assertEqual ("Normal square not where expected " ++ show pos) (Just (Normal Nothing)) (unoccupiedSquareAt emptyBoard pos)) expectedNormals






            

