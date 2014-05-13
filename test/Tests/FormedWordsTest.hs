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

            assertBool "Unexpected error in wordsFormedMidGame in attach left test initialisation" $ isRight formed
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

            assertEqual "Unexpected score for placed tile" ((1 + 1 + 1 + 1) * 2) overallscore
 
    attachRightWord :: Assertion
    attachRightWord =
        do
            let positions = take 5 $ catMaybes $ map posAt $ iterate (\(x,y) -> (x + 1,y)) (8,9)
            let tiles = [Letter 'E' 1, Letter 'L' 1, Letter 'L' 1, Letter 'O' 1, Blank (Just 'W')]
            let placed = M.fromList $ zip positions tiles

            let formed = wordsFormedMidGame testBoard placed

            assertBool "Unexpected error in wordsFormedMidGame in attach right test initialisation" $ isRight formed

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

            assertEqual "Unexpected score for placed tile" (4 + 1 + (2 * 1) + 1 + 1 + 0) overallscore

    attachAboveWord :: Assertion
    attachAboveWord =
        do
            let positions = catMaybes $ map posAt $ iterate (\(x,y) -> (x,y + 1)) (7,3)
            let tiles = [Letter 'A' 1, Letter 'B' 3]
            let placed = M.fromList $ zip positions tiles

            let formed = wordsFormedMidGame testBoard placed

            assertBool "Unexpected error in wordsFormedMidGame in attach above test initialisation" $ isRight formed

            let Right wordsFormed = formed

            assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["ABTELLY"]

            let squares = catMaybes $ map (squareAt testBoard) positions
            let expectedFormedWord = S.fromList $ zip positions $ zipWith putTileOn squares tiles ++ verticalSquares

            assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

            assertEqual "Unexpected player placed" (S.take 2 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

            assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

            let (overallscore, _) = wordsWithScores wordsFormed

            assertEqual "Unexpected score for placed tile" ((2 * 1) + 3 + 1 + 1 + 1 + 1 + 4) overallscore


    attachWordBelow :: Assertion
    attachWordBelow = 
        do
            let positions = take 2 $ catMaybes $ map posAt $ iterate (\(x,y) -> (x,y + 1)) (7,10)
            let tiles = [Letter 'A' 1, Letter 'B' 3]
            let placed = M.fromList $ zip positions tiles

            let formed = wordsFormedMidGame testBoard placed

            assertBool "Unexpected error in wordsFormedMidGame in attach above test initialisation" $ isRight formed

            let Right wordsFormed = formed

            assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["TELLYAB"]

            let squares = catMaybes $ map (squareAt testBoard) positions

            let leadingPositions = catMaybes $ map posAt $ iterate (\(x,y) -> (x,y + 1)) (7,5)
            let expectedFormedWord = S.fromList $ (zip leadingPositions verticalSquares) ++ (zip positions $ zipWith putTileOn squares tiles)

            assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

            assertEqual "Unexpected player placed" (S.drop 5 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

            assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

            let (overallscore, _) = wordsWithScores wordsFormed

            assertEqual "Unexpected score for placed tile" (1 + 1 + 1 + 1 + 4 + 1 + 3) overallscore


--    attachAboveAndBelow :: Assertion
--    attachAboveAndBelow =


    -- above and below
    -- left and right

    -- Parallel left
    -- Parallel Right
    -- Parallel below
    -- Parallel above

    -- Pass above
    -- Pass below
    -- Pass left
    -- Pass right












    isRight :: Either a b -> Bool
    isRight (Right _ ) = True
    isRight _ = False