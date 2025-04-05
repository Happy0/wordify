module Tests.Internationalisation.Spanish.LetterBagTest (makeSpanishBagTestSuccess) where

import Test.HUnit
import Test.HUnit.Base as H
import Wordify.Rules.LetterBag
import Wordify.Rules.LetterBag.Internal

spanishBagFilePath = "Config/spanishSet/bag.txt"

makeSpanishBagTestSuccess :: Assertion
makeSpanishBagTestSuccess = do
  letterBagResult <- makeBag spanishBagFilePath

  case letterBagResult of
    Left err -> H.assertFailure $ "makeBag returned an error " ++ show err
    Right letterBag -> do
      assertEqual "Expected 100 tiles in the bag" (length (validLetters letterBag)) 28