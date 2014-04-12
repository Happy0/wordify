module Tests.Regressions
    (
    	tests
    ) where

import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

import Tests.LetterBagTest

tests :: F.Test
tests = F.testGroup "Regressions" [
	
	F.testGroup "LetterBag"
		[
		F.testCase "Letter bag returns error when makeBag file is invalidly formatted" makeBagInvalidlyFormattedBag,
		F.testCase "Letter bag from file parsed correctly" makeBagTestSuccess,
		F.testCase "Letter bag returns error when makeBag file path invalid" makeBagInvalidPath]

	]