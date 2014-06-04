module Tests.Regressions
    (
        tests
    ) where

import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

import Tests.BoardTest
import Tests.LetterBagTest
import Tests.FormedWordsTest
import Tests.FullGameTest

tests :: F.Test
tests = F.testGroup "Regressions" [
    
    F.testGroup "LetterBag"
        [
            F.testCase "Letter bag returns error when makeBag file is invalidly formatted" makeBagInvalidlyFormattedBag,
            F.testCase "Letter bag from file parsed correctly" makeBagTestSuccess,
            F.testCase "Letter bag returns error when makeBag file path invalid" makeBagInvalidPath
        ],

    F.testGroup "Board"
        [
            F.testCase "allSquares function behaves as expected" allSquaresTest,
            F.testCase "Board letters left function behaves as expected" lettersLeftTest,
            F.testCase "Board letters right function behaves as expected" lettersRightTest,
            F.testCase "Board letters above function behaves as expected" lettersAboveTest,
            F.testCase "Board letters below function behaves as expected" lettersBelowTest,
            F.testCase "Tiles can not be placed on empty squares" tilesPlacedConsecutivelyTest,
            F.testCase "squareAt function behaves as expected" squareAtTest,
            F.testCase "occupiedSquareAt function behaves as expected" squareAtTest,
            F.testCase "occupiedSquareAt function behaves as expected where the square is unoccupied" occupiedSquareAtUnoccupiedTest,
            F.testCase "unoccupiedSquareAt function behaves as expected where the square is unoccupied" unoccupiedSquareAtTest,
            F.testCase "unoccupiedSquareAt function behaves as expected where the square is occupied" unoccupiedSquareAtTestOccupied,
            F.testCase "Bonus squres and normal squares are where they are expected on the board" boardCorrectlyFormed
        ],

    F.testGroup "FormedWord"
        [
            F.testCase "Words can be attached to the left of an existing word" attachLeftWord,
            F.testCase "Words can be attached to the right of an existing word" attachRightWord,
            F.testCase "Words can be attached to the top of an existing word" attachWordBelow,
            F.testCase "Words can be attached to the bottom of an existing word" attachAboveWord,
            F.testCase "Words can be attached to the top and bottom of an existing word" attachAboveAndBelow,
            F.testCase "Words can be attached to the left and right of an existing word" attachLeftAndRight,
            F.testCase "Words can be attached with adjacent words starting from the left" adjacentWordsLeft,
            F.testCase "Words can be attached with adjacent words starting from the right" adjacentWordsRight,
            F.testCase "Words can be attached with adjacent words starting from above" adjacentWordsAbove,
            F.testCase "Words can be attached with adjacent words starting from above" adjacentWordsBelow,
            F.testCase "Words can be formed from one letter starting from above" placedOneTileAbove,
            F.testCase "Words can be formed from one letter placed below" placedOneTileBelow,
            F.testCase "Words can be formed from one letter placed to the right of a word" placedOneTileRight,
            F.testCase "Words can be formed from one letter placed to the left of a word" placedOneTileLeft,
            F.testCase "Words can be formed by 'brushing' an existing word from the top" passesAbove,
            F.testCase "Words can be formed by 'brushing' an existing word from the bottom" passesBelow,
            F.testCase "Words formed can pass through two existing words" passesThroughTwoWords,
            F.testCase "A first word is valid if it passes through the star" firstWordThroughStar,
            F.testCase "A first word is invalid if it does not pass through the star" firstWordNotThroughStar,
            F.testCase "If a word does not connect with any words on the board, the expected error is returned" doesNotConnectWithWord,
            F.testCase "If a word forms a horizontal line, with one placed tile in the middle not in the line, the expected error is returned" nonContigiousHorizontal,
            F.testCase "If a word forms a vertical line, with one placed tile in the middle not in the line, the expected error is returned" nonContigiousVertical,
            F.testCase "Cannot placed a blank tile without giving it a letter" placeBlankNothing,
            F.testCase "Cannot placed a tile on a square which is already occupied" placeOnOccupiedSquare
        ],

    F.testGroup "PlayGameTest"
        [
            F.testCase "The full game playthrough test succeeds as expected" playThroughTest,
            F.testCase "The game is ended if all players skip twice consecutively" gameEndsOnConsecutiveSkips,
            F.testCase "The game is not ended if skips are not consecutive twice" gameDoesNotEndOnNonConsecutiveSkips,
            F.testCase "An exchange move behaves as expected" exchangeMoveExchangesLetters 
        ]

    ]
