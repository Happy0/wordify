module Tests.Regressions
    (
    	tests
    ) where

import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

import Tests.LetterBagTest
import Tests.BoardTest

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
			F.testCase "Board letters left function behaves as expected" lettersLeftTest,
			F.testCase "Board letters right function behaves as expected" lettersRightTest,
			F.testCase "Board letters above function behaves as expected" lettersAboveTest,
			F.testCase "Board letters below function behaves as expected" lettersBelowTest
		]

	]