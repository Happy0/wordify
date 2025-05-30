module Tests.Internationalisation.Spanish.LetterBagTest (makeSpanishBagTestSuccess) where

import Test.HUnit
import Test.HUnit.Base as H
import Wordify.Rules.LetterBag
import Wordify.Rules.LetterBag.Internal
import qualified Data.Map as M
import Wordify.Rules.Tile
import Data.Maybe(fromMaybe)

spanishBagFilePath = "Config/spanishSet/bag.txt"

countLetters :: [Tile] -> M.Map String Int
countLetters = foldr addTileCount M.empty 
  where
    addTileCount :: Tile -> M.Map String Int -> M.Map String Int
    addTileCount tile = M.insertWith (+) (fromMaybe "_" (tileString tile)) 1

makeSpanishBagTestSuccess :: Assertion
makeSpanishBagTestSuccess = do
  letterBagResult <- makeBag spanishBagFilePath

  let expectedTileQuantities =
        [ ("A", 11)
        , ("B", 3)
        , ("C", 4)
        , ("D", 4)
        , ("E", 11)
        , ("F", 2)
        , ("G", 2)
        , ("H", 2)
        , ("I", 6)
        , ("J", 2)
        , ("K", 1)
        , ("L", 4)
        , ("LL", 1)
        , ("M", 3)
        , ("N", 5)
        , ("O", 8)
        , ("P", 2)
        , ("Q", 1)
        , ("R", 4)
        , ("RR", 1)
        , ("S", 7)
        , ("T", 4)
        , ("U", 6)
        , ("V", 2)
        , ("W", 1)
        , ("X", 1)
        , ("Y", 1)
        , ("Z", 1)
        , ("_", 2)
        , ("Ñ", 1)
        ]
  

  case letterBagResult of
    Left err -> H.assertFailure $ "makeBag returned an error " ++ show err
    Right letterBag -> do
      assertEqual "Expected 28 different valid letters in the bag" (length (validLetters letterBag)) 29

      let takeResult = takeLetters letterBag 103

      case takeResult of
        Nothing -> H.assertFailure $ "takeLetters returned Nothing, Expected Just"
        Just (tiles, newBag) -> do
          assertEqual "Expected 103 tiles to be taken from the bag" (length tiles) 103
          assertEqual "Expect no letters to left in the bag" (bagSize newBag) 0
          let actualTileQuantities = M.toList (countLetters tiles)
          assertEqual "Expected the tiles" expectedTileQuantities actualTileQuantities