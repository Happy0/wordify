module Tests.FormedWordsTest where

    import Tests.SharedTestData
    import Pos
    import Tile
    import Board
    import Board.Internal
    import qualified Data.Map as M
    import FormedWord
    import Test.HUnit.Base
    import Data.Maybe
    import Data.Either
    import Pos.Internal
    import qualified Data.Sequence as S
    import Square
    import Control.Applicative

    testBoard :: Board
    testBoard = Board squareMap
        where
            squareMap = M.fromList $ (M.assocs emptySquares ++ verticals ++ horizontals)
            Board (emptySquares) = emptyBoard

    attachLeftWord :: Assertion
    attachLeftWord =
        do
            let positions = take 3 $ catMaybes $ map posAt $ iterate(\(x,y) -> (x + 1,y)) (4,5)
            let tiles = [Letter 'T' 1, Letter 'E' 1, Letter 'S' 1]
            let placed = M.fromList $ zip positions tiles

            let formed = wordsFormedMidGame testBoard placed

            assertBool "Unexpected error in wordsFormedMidGame in attach left test initialisation" $ isValid formed
            let Right wordsFormed = formed

            let expectedWord = S.fromList $ M.toList placed ++ [(Pos 7 5 "G5", Letter 'T' 1)]
            assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["TEST"]

            let squares = catMaybes $ map (squareAt testBoard) positions
            let expectedSquares = zipWith putTileOn squares tiles

            let expectedFormedWord = S.fromList $ zip positions expectedSquares ++ [(Pos 7 5 "G5", Normal $ Just $ Letter 'T' 1)]

            assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

            assertEqual "Unexpected player placed" (S.take 3 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

            assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

            let (overallscore, _) = wordsWithScores wordsFormed

            assertEqual "Unexpected score for placed tiles" ((1 + 1 + 1 + 1) * 2) overallscore
 
    attachRightWord :: Assertion
    attachRightWord =
        do
            let positions = take 5 $ catMaybes $ map posAt $ iterate (\(x,y) -> (x + 1,y)) (8,9)
            let tiles = [Letter 'E' 1, Letter 'L' 1, Letter 'L' 1, Letter 'O' 1, Blank (Just 'W')]
            let placed = M.fromList $ zip positions tiles

            let formed = wordsFormedMidGame testBoard placed

            assertBool "Unexpected error in wordsFormedMidGame in attach right test initialisation" $ isValid formed

            let Right wordsFormed = formed

            let expectedWord = S.fromList $ (Pos 7 9 "G7", Letter 'Y' 4) : M.toList placed

            assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["YELLOW"]

            let squares = catMaybes $ map (squareAt testBoard) positions
            let expectedSquares = zipWith putTileOn squares tiles
            
            let expectedFormedWord = S.fromList $ ((Pos 7 9 "G9"), (DoubleLetter $ Just $ Letter 'Y' 4)) : zip positions expectedSquares

            assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

            assertEqual "Unexpected player placed" (S.drop 1 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

            assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

            let (overallscore, _) = wordsWithScores wordsFormed

            assertEqual "Unexpected score for placed tiles" (4 + 1 + (2 * 1) + 1 + 1 + 0) overallscore

    attachAboveWord :: Assertion
    attachAboveWord =
        do
            let positions = catMaybes $ map posAt $ iterate (\(x,y) -> (x,y + 1)) (7,3)
            let tiles = [Letter 'A' 1, Letter 'B' 3]
            let placed = M.fromList $ zip positions tiles

            let formed = wordsFormedMidGame testBoard placed

            assertBool "Unexpected error in wordsFormedMidGame in attach above test initialisation" $ isValid formed

            let Right wordsFormed = formed

            assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["ABTELLY"]

            let squares = catMaybes $ map (squareAt testBoard) positions
            let expectedFormedWord = S.fromList $ zip positions $ zipWith putTileOn squares tiles ++ verticalSquares

            assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

            assertEqual "Unexpected player placed" (S.take 2 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

            assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

            let (overallscore, _) = wordsWithScores wordsFormed

            assertEqual "Unexpected score for placed tiles" ((2 * 1) + 3 + 1 + 1 + 1 + 1 + 4) overallscore


    attachWordBelow :: Assertion
    attachWordBelow = 
        do
            let positions = take 2 $ catMaybes $ map posAt $ iterate (\(x,y) -> (x,y + 1)) (7,10)
            let tiles = [Letter 'A' 1, Letter 'B' 3]
            let placed = M.fromList $ zip positions tiles

            let formed = wordsFormedMidGame testBoard placed

            assertBool "Unexpected error in wordsFormedMidGame in attach above test initialisation" $ isValid formed

            let Right wordsFormed = formed

            assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["TELLYAB"]

            let squares = catMaybes $ map (squareAt testBoard) positions

            let leadingPositions = catMaybes $ map posAt $ iterate (\(x,y) -> (x,y + 1)) (7,5)
            let expectedFormedWord = S.fromList $ (zip leadingPositions verticalSquares) ++ (zip positions $ zipWith putTileOn squares tiles)

            assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

            assertEqual "Unexpected player placed" (S.drop 5 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

            assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

            let (overallscore, _) = wordsWithScores wordsFormed

            assertEqual "Unexpected score for placed tiles" (1 + 1 + 1 + 1 + 4 + 1 + 3) overallscore


    attachAboveAndBelow :: Assertion
    attachAboveAndBelow =
        do
            let abovePositions = catMaybes $ map posAt $ [(7,3), (7,4)]
            let belowPositions = catMaybes $ map posAt $ [(7,10), (7,11)]
            let placedPositions = (abovePositions ++ belowPositions)
            let tiles = cycle $ [Letter 'A' 1, Letter 'B' 3]
            let placed = M.fromList $ zip placedPositions tiles

            let formed = wordsFormedMidGame testBoard placed

            assertBool "Unexpected error in wordsFormedMidGame in attach above test initialisation" $ isValid formed

            let Right wordsFormed = formed

            assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["ABTELLYAB"]

            let placedSquares = catMaybes $ map (squareAt testBoard) placedPositions
            let tilesOnPlaced = zipWith putTileOn placedSquares tiles

            let expectedFormedWord = S.fromList $ zip abovePositions (take 2 tilesOnPlaced) ++ (zip verticalPositions verticalSquares) ++ (zip belowPositions (drop 2 tilesOnPlaced))

            assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

            assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

            let (overallscore, _) = wordsWithScores wordsFormed

            assertEqual "Unexpected score for placed tiles" ((2 * 1) + 3 + 1 + 1 + 1 + 1 + 4 + 1 + 3) overallscore


    attachLeftAndRight :: Assertion
    attachLeftAndRight =
        do
            let leftPositions = catMaybes $ map posAt $ [(3,7), (4,7)]
            let rightPositions = catMaybes $ map posAt $ [(10,7), (11,7)]
            let placedPositions = (leftPositions ++ rightPositions)
            let tiles = cycle $ [Letter 'A' 1, Letter 'B' 3]
            let placed = M.fromList $ zip placedPositions tiles

            let formed = wordsFormedMidGame testBoard placed

            assertBool "Unexpected error in wordsFormedMidGame in attach above test initialisation" $ isValid formed

            let Right wordsFormed = formed

            assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["ABHELLOAB"]

            let placedSquares = catMaybes $ map (squareAt testBoard) placedPositions
            let tilesOnPlaced = zipWith putTileOn placedSquares tiles

            let expectedFormedWord = S.fromList $ zip leftPositions (take 2 tilesOnPlaced) ++ (zip horizontalPositions horizontalSquares) ++ (zip rightPositions (drop 2 tilesOnPlaced))

            assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

            assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

            let (overallscore, _) = wordsWithScores wordsFormed

            assertEqual "Unexpected score for placed tiles" ((2 * 1) + 3 + 4 + 1 + 1 + 1 + 1 + 1 + 3) overallscore



    -- Adjacent words left
    -- Adjacent words Right
    -- Adjacent words below
    -- Adjacent words above

    -- Drop down and connect

    -- Pass above
    -- Pass below
    -- Pass left
    -- Pass right

    -- first word valid
    -- first word invalid

    -- non contigious word horizontal
    -- non contigious word vertical

    -- Place Blank Nothing












    isValid :: Either a b -> Bool
    isValid (Right _ ) = True
    isValid _ = False