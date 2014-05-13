module Tests.Regressions
    (
    	tests
    ) where

import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

import Tests.LetterBagTest
import Tests.BoardTest
import Tests.FormedWordsTest

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
			F.testCase "unoccupiedSquareAt function behaves as expected where the square is occupied" unoccupiedSquareAtTestOccupied
		],

	F.testGroup "FormedWord"
		[
			F.testCase "Words can be attached to the left of an existing word" attachLeftWord,
			F.testCase "Words can be attached to the right of an existing word" attachRightWord,
			F.testCase "Words can be attached to the top of an existing word" attachWordBelow,
			F.testCase "Words can be attached to the bottom of an existing word" attachAboveWord
		]

	]