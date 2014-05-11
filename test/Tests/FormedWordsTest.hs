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

    attachRightWord :: Assertion
    attachRightWord =
        do
            let positions = catMaybes $ map posAt $ iterate (\(x,y) -> (x + 1,y)) (8,9)
            let tiles = [Letter 'E' 1, Letter 'L' 1, Letter 'L' 1, Letter 'O' 1, Blank (Just 'W')]
            let placed = M.fromList $ positions `zip` tiles

            let formed = wordsFormedMidGame testBoard placed

            assertBool "Unexpected scrabble error in wordsFormedMidGame in attach right test" $ isRight formed

            let Right wordsFormed = formed

            let expectedWord = S.fromList $ (Pos 7 9 "G7", Letter 'Y' 4) : M.toList placed

            assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["YELLOW"]

            let squares = catMaybes $ map (squareAt testBoard) (take 5 positions)
            let expectedSquares = zipWith putTileOn squares tiles
            
            let expectedFormedWord = S.fromList $ ((Pos 7 9 "G9"), (DoubleLetter $ Just $ Letter 'Y' 4)) : zip (take 5 positions) expectedSquares

            assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

            assertEqual "Unexpected player placed" (S.drop 1 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

            assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

    -- Left word

    -- AboveWord
    -- BelowWord

    -- Parallel left
    -- Parallel Right
    -- Parallel below
    -- Parallel above

    -- above and below
    -- left and right

    -- Perpendicular

    -- Pass above
    -- Pass below
    -- Pass left
    -- Pass right












    isRight :: Either a b -> Bool
    isRight (Right _ ) = True
    isRight _ = False