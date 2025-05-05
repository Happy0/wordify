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
      assertEqual "Expected 28 different valid letters in the bag" (length (validLetters letterBag)) 28

      let takeResult = takeLetters letterBag 103

      case takeResult of
        Nothing -> H.assertFailure $ "takeLetters returned Nothing, Expected Just"
        Just (tiles, newBag) -> do
          assertEqual "Expected 103 tiles to be taken from the bag" (length tiles) 103
          assertEqual "Expect no letters to left in the bag" (bagSize newBag) 0
