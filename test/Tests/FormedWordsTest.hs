module Tests.FormedWordsTest where

import Control.Applicative
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as S
import Test.HUnit.Base
import Tests.SharedTestData
import Wordify.Rules.Board
import Wordify.Rules.Board.Internal
import Wordify.Rules.FormedWord
import Wordify.Rules.Pos
import Wordify.Rules.Pos.Internal
import Wordify.Rules.ScrabbleError
import Wordify.Rules.Square
import Wordify.Rules.Tile

testBoard :: Board
testBoard = Board squareMap
  where
    squareMap = M.fromList $ (M.assocs emptySquares ++ verticals ++ horizontals)
    Board (emptySquares) = emptyBoard

{-
    Asserts that we can correctly add the bracket notation to a placed word, prepending to a word
 -}
testPrettyPrintIntersectionPrepend :: Assertion
testPrettyPrintIntersectionPrepend =
  do
    let positions = take 3 $ catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (4, 5)
    let tiles = [Letter "T" 1, Letter "E" 1, Letter "S" 1]
    let placedList = zip positions $ map (Normal . Just) tiles
    let placed = M.fromList placedList

    let formedPositions = catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (8, 5)
    let formed = (S.fromList placedList) S.>< (S.fromList $ zip formedPositions $ map (Normal . Just . flip Letter 1) ["T", "I", "N", "G"])

    let actual = prettyPrintIntersections placed formed

    assertEqual "Did not form expected pretty printed intersection" "TES(TING)" actual

testPrettyPrintIntersectionAppend :: Assertion
testPrettyPrintIntersectionAppend =
  do
    let positions = take 4 $ catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (8, 5)

    let tiles = map (flip Letter 1) ["T", "I", "N", "G"]
    let placedList = zip positions $ map (Normal . Just) tiles
    let placed = M.fromList placedList

    let formedPositions = take 3 $ catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (4, 5)
    let formed = (S.fromList $ zip formedPositions $ map (Normal . Just . flip Letter 1) ["T", "E", "S"]) S.>< S.fromList placedList

    let actual = prettyPrintIntersections placed formed

    assertEqual "Did not form expected pretty printed intersection" "(TES)TING" actual

testPrettyPrintIntersectionFirstWord :: Assertion
testPrettyPrintIntersectionFirstWord =
  do
    let positions = take 4 $ catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (8, 5)
    let tiles = map (flip Letter 1) ["T", "E", "S", "T"]
    let placedList = zip positions $ map (Normal . Just) tiles
    let placed = M.fromList placedList

    let formed = S.fromList $ zip positions $ map (Normal . Just) tiles

    let actual = prettyPrintIntersections placed formed
    assertEqual "Did not form expected pretty printed intersection" "TEST" actual

testPrettyPrintThroughPlacedLetters :: Assertion
testPrettyPrintThroughPlacedLetters =
  do
    let positions = take 4 $ catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (8, 5)
    let positions2 = take 2 $ catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (13, 5)

    let tiles = map (flip Letter 1) ["T", "I", "N", "G"]
    let placedList = zip positions $ map (Normal . Just) tiles
    let placed = M.fromList placedList

    let alreadyPlacedList2 = zip positions2 $ map (Normal . Just) tiles

    let formedPositions = take 3 $ catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (4, 5)
    let formed = (S.fromList $ zip formedPositions $ map (Normal . Just . flip Letter 1) ["T", "E", "S"]) S.>< S.fromList placedList S.>< S.fromList alreadyPlacedList2

    let actual = prettyPrintIntersections placed formed

    assertEqual "Did not form expected pretty printed intersection" "(TES)TING(TI)" actual

attachLeftWord :: Assertion
attachLeftWord =
  do
    let positions = take 3 $ catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (4, 5)
    let tiles = [Letter "T" 1, Letter "E" 1, Letter "S" 1]
    let placed = M.fromList $ zip positions tiles

    let formed = wordsFormedMidGame testBoard placed

    assertBool "Unexpected error in wordsFormedMidGame in attach left test initialisation" $ isValid formed
    let Right wordsFormed = formed

    let expectedWord = S.fromList $ M.toList placed ++ [(Pos 7 5 "G5", Letter "T" 1)]
    assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["TEST"]

    let squares = catMaybes $ map (unoccupiedSquareAt testBoard) positions
    let expectedSquares = zipWith putTileOn squares tiles

    let expectedFormedWord = S.fromList $ zip positions expectedSquares ++ [(Pos 7 5 "G5", Normal $ Just $ Letter "T" 1)]

    assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

    assertEqual "Unexpected player placed" (S.take 3 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

    assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

    let (overallscore, _) = wordsWithScores wordsFormed

    assertEqual "Unexpected score for placed tiles" ((1 + 1 + 1 + 1) * 2) overallscore

attachRightWord :: Assertion
attachRightWord =
  do
    let positions = take 5 $ catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (8, 9)
    let tiles = [Letter "E" 1, Letter "L" 1, Letter "L" 1, Letter "O" 1, Blank (Just "W")]
    let placed = M.fromList $ zip positions tiles

    let formed = wordsFormedMidGame testBoard placed

    assertBool "Unexpected error in wordsFormedMidGame in attach right test initialisation" $ isValid formed

    let Right wordsFormed = formed

    let expectedWord = S.fromList $ (Pos 7 9 "G7", Letter "Y" 4) : M.toList placed

    assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["YELLOW"]

    let squares = catMaybes $ map (unoccupiedSquareAt testBoard) positions
    let expectedSquares = zipWith putTileOn squares tiles

    let expectedFormedWord = S.fromList $ ((Pos 7 9 "G9"), (DoubleLetter $ Just $ Letter "Y" 4)) : zip positions expectedSquares

    assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

    assertEqual "Unexpected player placed" (S.drop 1 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

    assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

    let (overallscore, _) = wordsWithScores wordsFormed

    assertEqual "Unexpected score for placed tiles" (4 + 1 + (2 * 1) + 1 + 1 + 0) overallscore

attachAboveWord :: Assertion
attachAboveWord =
  do
    let positions = catMaybes $ map posAt $ iterate (\(x, y) -> (x, y + 1)) (7, 3)
    let tiles = [Letter "A" 1, Letter "B" 3]
    let placed = M.fromList $ zip positions tiles

    let formed = wordsFormedMidGame testBoard placed

    assertBool "Unexpected error in wordsFormedMidGame in attach above test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["ABTELLY"]

    let squares = catMaybes $ map (unoccupiedSquareAt testBoard) positions
    let expectedFormedWord = S.fromList $ zip positions $ zipWith putTileOn squares tiles ++ verticalSquares

    assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

    assertEqual "Unexpected player placed" (S.take 2 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

    assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

    let (overallscore, _) = wordsWithScores wordsFormed

    assertEqual "Unexpected score for placed tiles" ((2 * 1) + 3 + 1 + 1 + 1 + 1 + 4) overallscore

attachWordBelow :: Assertion
attachWordBelow =
  do
    let positions = take 2 $ catMaybes $ map posAt $ iterate (\(x, y) -> (x, y + 1)) (7, 10)
    let tiles = [Letter "A" 1, Letter "B" 3]
    let placed = M.fromList $ zip positions tiles

    let formed = wordsFormedMidGame testBoard placed

    assertBool "Unexpected error in wordsFormedMidGame in attach below test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["TELLYAB"]

    let squares = catMaybes $ map (unoccupiedSquareAt testBoard) positions

    let leadingPositions = catMaybes $ map posAt $ iterate (\(x, y) -> (x, y + 1)) (7, 5)
    let expectedFormedWord = S.fromList $ (zip leadingPositions verticalSquares) ++ (zip positions $ zipWith putTileOn squares tiles)

    assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

    assertEqual "Unexpected player placed" (S.drop 5 expectedFormedWord) (S.fromList $ (playerPlaced wordsFormed))

    assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

    let (overallscore, _) = wordsWithScores wordsFormed

    assertEqual "Unexpected score for placed tiles" (1 + 1 + 1 + 1 + 4 + 1 + 3) overallscore

attachAboveAndBelow :: Assertion
attachAboveAndBelow =
  do
    let abovePositions = catMaybes $ map posAt $ [(7, 3), (7, 4)]
    let belowPositions = catMaybes $ map posAt $ [(7, 10), (7, 11)]
    let placedPositions = (abovePositions ++ belowPositions)
    let tiles = cycle $ [Letter "A" 1, Letter "B" 3]
    let placed = M.fromList $ zip placedPositions tiles

    let formed = wordsFormedMidGame testBoard placed

    assertBool "Unexpected error in wordsFormedMidGame in attach above test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["ABTELLYAB"]

    let placedSquares = catMaybes $ map (unoccupiedSquareAt testBoard) placedPositions
    let tilesOnPlaced = zipWith putTileOn placedSquares tiles

    let expectedFormedWord = S.fromList $ zip abovePositions (take 2 tilesOnPlaced) ++ (zip verticalPositions verticalSquares) ++ (zip belowPositions (drop 2 tilesOnPlaced))

    assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

    assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

    let (overallscore, _) = wordsWithScores wordsFormed

    assertEqual "Unexpected score for placed tiles" ((2 * 1) + 3 + 1 + 1 + 1 + 1 + 4 + 1 + 3) overallscore

attachLeftAndRight :: Assertion
attachLeftAndRight =
  do
    let leftPositions = catMaybes $ map posAt $ [(3, 7), (4, 7)]
    let rightPositions = catMaybes $ map posAt $ [(10, 7), (11, 7)]
    let placedPositions = (leftPositions ++ rightPositions)
    let tiles = cycle $ [Letter "A" 1, Letter "B" 3]
    let placed = M.fromList $ zip placedPositions tiles

    let formed = wordsFormedMidGame testBoard placed

    assertBool "Unexpected error in wordsFormedMidGame test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertBool "Unexpected words formed " $ (wordStrings wordsFormed) == ["ABHELLOAB"]

    let placedSquares = catMaybes $ map (unoccupiedSquareAt testBoard) placedPositions
    let tilesOnPlaced = zipWith putTileOn placedSquares tiles

    let expectedFormedWord = S.fromList $ zip leftPositions (take 2 tilesOnPlaced) ++ (zip horizontalPositions horizontalSquares) ++ (zip rightPositions (drop 2 tilesOnPlaced))

    assertEqual "Unexpected main word formed" expectedFormedWord (mainWord wordsFormed)

    assertEqual "Expected empty adjecent words" [] (adjacentWords wordsFormed)

    let (overallscore, _) = wordsWithScores wordsFormed

    assertEqual "Unexpected score for placed tiles" ((2 * 1) + 3 + 4 + 1 + 1 + 1 + 1 + 1 + 3) overallscore

adjacentWordsLeft :: Assertion
adjacentWordsLeft =
  do
    let positions = catMaybes $ map posAt [(8, 5), (8, 6), (8, 8)]
    let tiles = [Letter "O" 1, Letter "I" 1, Letter "S" 1]
    let placedList = zip positions tiles
    let placed = M.fromList $ placedList

    let formed = wordsFormedMidGame testBoard placed

    assertBool "Unexpected error in wordsFormedMidGame test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["OILS", "TO", "EI", "LS"] (wordStrings wordsFormed)

    let squares = zipWith putTileOn (catMaybes $ map (unoccupiedSquareAt testBoard) positions) tiles
    let positionsFromTop = catMaybes $ map posAt $ iterate (\(x, y) -> (x, y + 1)) (8, 5)
    let squaresWithPositions = zip positionsFromTop $ (init squares) ++ [Normal $ Just $ Letter "L" 1] ++ [last squares]
    let expectedMainWord = S.fromList $ squaresWithPositions

    assertEqual "Unexpected main word" expectedMainWord (mainWord wordsFormed)

    let expectedWordsWithScores = (((1 + 1 + 1 + 1) * 2) + 2 + 2 + 4, [("OILS", ((1 + 1 + 1 + 1) * 2)), ("TO", 2), ("EI", 2), ("LS", 4)])

    assertEqual "Unexpected words with scores" expectedWordsWithScores (wordsWithScores wordsFormed)

    let connectedTo = take 2 verticals ++ drop 3 verticals
    let placedSquares = zip positions $ (init squares) ++ [last squares]
    let expectedAdjacent = zipWith (\l r -> l S.<| S.singleton r) connectedTo placedSquares

    assertEqual "Unexpected adjacent words" expectedAdjacent (adjacentWords wordsFormed)

adjacentWordsRight :: Assertion
adjacentWordsRight =
  do
    let positions = catMaybes $ map posAt [(6, 4), (6, 5), (6, 6), (6, 8)]
    let tiles = [Letter "B" 3, Letter "I" 1, Letter "T" 1, Letter "R" 1]
    let placedList = zip positions tiles
    let placed = M.fromList $ placedList

    let formed = wordsFormedMidGame testBoard placed

    assertBool "Unexpected error in wordsFormedMidGame test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["BITER", "IT", "TE", "RL"] (wordStrings wordsFormed)

    let squares = zipWith putTileOn (catMaybes $ map (unoccupiedSquareAt testBoard) positions) tiles
    let positionsFromTop = catMaybes $ map posAt $ iterate (\(x, y) -> (x, y + 1)) (6, 4)
    let squaresWithPositions = zip positionsFromTop $ (init squares) ++ [Normal $ Just $ Letter "E" 1] ++ [last squares]
    let expectedMainWord = S.fromList $ squaresWithPositions

    assertEqual "Unexpected main word" expectedMainWord (mainWord wordsFormed)

    let expectedWordsWithScores = ((3 + 1 + 3 + 1 + 1) + 2 + 4 + 2, [("BITER", (3 + 1 + 3 + 1 + 1)), ("IT", 2), ("TE", 4), ("RL", 2)])

    assertEqual "Unexpected words with scores" expectedWordsWithScores (wordsWithScores wordsFormed)

    let connectedTo = take 2 verticals ++ drop 3 verticals
    let placedSquares = zip positions $ (init squares) ++ [last squares]
    let expectedAdjacent = zipWith (\l r -> l S.<| S.singleton r) (drop 1 placedSquares) connectedTo

    assertEqual "Unexpected adjacent words" expectedAdjacent (adjacentWords wordsFormed)

adjacentWordsAbove :: Assertion
adjacentWordsAbove =
  do
    let positions = catMaybes $ map posAt [(6, 6), (8, 6)]
    let tiles = [Letter "H" 4, Letter "J" 8]
    let placedList = zip positions tiles
    let placed = M.fromList $ placedList

    let formed = wordsFormedMidGame testBoard placed

    assertBool "Unexpected error in wordsFormedMidGame test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["HEJ", "HE", "JL"] (wordStrings wordsFormed)

    let squares = zipWith putTileOn (catMaybes $ map (unoccupiedSquareAt testBoard) positions) tiles
    let positionsFromLeft = catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (6, 6)
    let squaresWithPositions = zip positionsFromLeft $ (init squares) ++ [Normal $ Just $ Letter "E" 1] ++ [last squares]
    let expectedMainWord = S.fromList $ squaresWithPositions

    assertEqual "Unexpected main word" expectedMainWord (mainWord wordsFormed)

    let expectedWordsWithScores = (((4 * 3) + 1 + 8) + 13 + 9, [("HEJ", ((4 * 3) + 1 + 8)), ("HE", 13), ("JL", 9)])

    assertEqual "Unexpected words with scores" expectedWordsWithScores (wordsWithScores wordsFormed)

    let connectedTo = [head (drop 1 horizontals)] ++ [head $ drop 3 horizontals]
    let placedSquares = zip positions $ (init squares) ++ [last squares]
    let expectedAdjacent = zipWith (\l r -> l S.<| S.singleton r) placedSquares connectedTo

    assertEqual "Unexpected adjacent words" expectedAdjacent (adjacentWords wordsFormed)

adjacentWordsBelow :: Assertion
adjacentWordsBelow =
  do
    let positions = catMaybes $ map posAt [(6, 8), (8, 8)]
    let tiles = [Letter "I" 1, Letter "L" 1]
    let placedList = zip positions tiles
    let placed = M.fromList $ placedList

    let formed = wordsFormedMidGame testBoard placed

    assertBool "Unexpected error in wordsFormedMidGame test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["ILL", "EI", "LL"] (wordStrings wordsFormed)

    let squares = zipWith putTileOn (catMaybes $ map (unoccupiedSquareAt testBoard) positions) tiles
    let positionsFromLeft = catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (6, 8)
    let squaresWithPositions = zip positionsFromLeft $ (init squares) ++ [Normal $ Just $ Letter "L" 1] ++ [last squares]
    let expectedMainWord = S.fromList $ squaresWithPositions

    assertEqual "Unexpected main word" expectedMainWord (mainWord wordsFormed)

    let expectedWordsWithScores = (6 + 2 + 4, [("ILL", 6), ("EI", 2), ("LL", 4)])

    assertEqual "Unexpected words with scores" expectedWordsWithScores (wordsWithScores wordsFormed)

    let connectedTo = [head (drop 1 horizontals)] ++ [head $ drop 3 horizontals]
    let placedSquares = zip positions $ (init squares) ++ [last squares]
    let expectedAdjacent = zipWith (\l r -> l S.<| S.singleton r) connectedTo placedSquares

    assertEqual "Unexpected adjacent words" expectedAdjacent (adjacentWords wordsFormed)

placedOneTileAbove :: Assertion
placedOneTileAbove =
  do
    let placed = catMaybes $ map posAt [(9, 6)]
    let tiles = [Letter "Y" 4]
    let placedTiles = M.fromList $ zip placed tiles

    let formed = wordsFormedMidGame testBoard placedTiles

    assertBool "Unexpected error in wordsFormedMidGame test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["YO"] (wordStrings wordsFormed)

placedOneTileBelow :: Assertion
placedOneTileBelow =
  do
    let placed = catMaybes $ map posAt [(9, 8)]
    let tiles = [Letter "I" 1]
    let placedTiles = M.fromList $ zip placed tiles

    let formed = wordsFormedMidGame testBoard placedTiles

    assertBool "Unexpected error in wordsFormedMidGame test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["OI"] (wordStrings wordsFormed)

placedOneTileRight :: Assertion
placedOneTileRight =
  do
    let placed = catMaybes $ map posAt [(8, 5)]
    let tiles = [Letter "O" 1]
    let placedTiles = M.fromList $ zip placed tiles

    let formed = wordsFormedMidGame testBoard placedTiles

    assertBool "Unexpected error in wordsFormedMidGame test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["TO"] (wordStrings wordsFormed)

placedOneTileLeft :: Assertion
placedOneTileLeft =
  do
    let placed = catMaybes $ map posAt [(6, 9)]
    let tiles = [Letter "O" 1]
    let placedTiles = M.fromList $ zip placed tiles

    let formed = wordsFormedMidGame testBoard placedTiles

    assertBool "Unexpected error in wordsFormedMidGame test initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["OY"] (wordStrings wordsFormed)

passesAbove :: Assertion
passesAbove =
  do
    let positions = catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (6, 4)
    let tiles = [Letter "H" 4, Letter "A" 1, Letter "S" 1]
    let placedTiles = M.fromList $ zip positions tiles

    let formed = wordsFormedMidGame testBoard placedTiles

    assertBool "Unexpected error in initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["HAS", "ATELLY"] (wordStrings wordsFormed)

passesBelow :: Assertion
passesBelow =
  do
    let positions = catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (6, 10)
    let tiles = [Letter "H" 4, Letter "A" 1, Letter "S" 1]
    let placedTiles = M.fromList $ zip positions tiles

    let formed = wordsFormedMidGame testBoard placedTiles

    assertBool "Unexpected error in initialisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["HAS", "TELLYA"] (wordStrings wordsFormed)

passesThroughTwoWords :: Assertion
passesThroughTwoWords =
  do
    let setupPositions = catMaybes $ map posAt $ iterate (\(x, y) -> (x, y + 1)) (9, 8)
    let setupSquares = [Normal $ Just $ Letter "F" 4, Normal $ Just $ Letter "F" 4]

    let boardSetup = Board $ M.fromList $ (allSquares testBoard) ++ zip setupPositions setupSquares

    let placePositions = catMaybes $ map posAt [(8, 9), (10, 9)]
    let tiles = [Letter "O" 1, Letter "O" 1]
    let placedTiles = M.fromList $ zip placePositions tiles

    let formed = wordsFormedMidGame boardSetup placedTiles

    assertBool "Unexpected error in initilisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed" ["YOFO"] (wordStrings wordsFormed)

    assertEqual "Unexpected score for word formed" (10, [("YOFO", 10)]) (wordsWithScores wordsFormed)

firstWordThroughStar :: Assertion
firstWordThroughStar =
  do
    let positions = catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (6, 8)
    let tiles = map (\lett -> Letter [lett] 1) "LAST"
    let placedList = zip positions tiles
    let placed = M.fromList $ placedList

    let formed = wordFormedFirstMove emptyBoard placed

    assertBool "Unexpected error in initilisation" $ isValid formed

    let Right wordsFormed = formed

    assertEqual "Unexpected words formed by valid first move" (wordsWithScores wordsFormed) (8, [("LAST", 8)])

firstWordNotThroughStar :: Assertion
firstWordNotThroughStar =
  do
    let positions = catMaybes $ map posAt $ iterate (\(x, y) -> (x, y + 1)) (8, 9)
    let tiles = map (\lett -> Letter [lett] 1) "LAST"
    let placedList = zip positions tiles
    let placed = M.fromList $ placedList

    let formed = wordFormedFirstMove emptyBoard placed

    assertEqual "Unexpected result for placing tiles which do not intersect the star on the first move" (Left DoesNotCoverTheStarTile) formed

firstWordNotContigiousWord :: Assertion
firstWordNotContigiousWord =
  do
    let positions = catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (6, 8)
    let tiles = map (\lett -> Letter [lett] 1) "LAST"
    let placedList = zip positions tiles
    let placed = M.fromList $ (take 1 placedList) ++ (drop 2 placedList)

    let formed = wordFormedFirstMove emptyBoard placed

    assertEqual "Unexpected error when placing tiles which are not in a connected line " (Left $ MisplacedLetter (Pos 8 8 "H8")) formed

doesNotConnectWithWord :: Assertion
doesNotConnectWithWord =
  do
    let positions = catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (4, 15)
    let tiles = map (\lett -> Letter [lett] 1) "LAST"
    let placedList = zip positions tiles
    let placed = M.fromList $ placedList

    let formed = wordsFormedMidGame emptyBoard placed

    assertEqual "Placing tiles that do not connect with a word does not throw the expected error" (Left DoesNotConnectWithWord) formed

nonContigiousHorizontal :: Assertion
nonContigiousHorizontal =
  do
    let positions = catMaybes $ map posAt [(5, 9), (6, 9), (8, 10), (9, 9)]
    let tiles = [Letter "T" 1, Letter "O" 1, Letter "E" 1, Letter "D" 2]
    let placedList = zip positions tiles
    let placed = M.fromList placedList

    let formed = wordsFormedMidGame testBoard placed

    assertEqual "Unexpected outcome when placing tiles which are not in a connected line while passing through a word" (Left $ MisplacedLetter (Pos 8 10 "H10")) formed

nonContigiousVertical :: Assertion
nonContigiousVertical =
  do
    let positions = catMaybes $ map posAt [(9, 6), (9, 8), (10, 9), (9, 10)]
    let tiles = [Letter "T" 1, Letter "Y" 1, Letter "E" 1, Letter "D" 2]
    let placedList = zip positions tiles
    let placed = M.fromList placedList

    let formed = wordsFormedMidGame testBoard placed

    assertEqual "Unexpected outcome when placing tiles which are not in a connected line while passing through a word" (Left $ MisplacedLetter (Pos 10 9 "J9")) formed

placeOnOccupiedSquare :: Assertion
placeOnOccupiedSquare =
  do
    let tiles = [Letter "T" 1, Blank Nothing, Letter "A" 1]

    let placed = M.fromList $ zip verticalPositions tiles

    let formed = wordsFormedMidGame testBoard placed

    assertEqual "Unexpected outcome for tiles placed on an already occupied square" (Left $ PlacedTileOnOccupiedSquare (head verticalPositions) (head tiles)) formed
