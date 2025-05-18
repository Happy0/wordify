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
        [ ("A", 9)
        , ("B", 2)
        , ("C", 2)
        , ("CH", 4)
        , ("D", 4)
        , ("E", 12)
        , ("F", 2)
        , ("G", 3)
        , ("H", 2)
        , ("I", 9)
        , ("J", 1)
        , ("K", 1)
        , ("L", 4)
        , ("LL", 3)
        , ("M", 2)
        , ("N", 6)
        , ("O", 8)
        , ("P", 2)
        , ("Q", 1)
        , ("R", 6)
        , ("S", 4)
        , ("T", 6)
        , ("U", 4)
        , ("V", 2)
        , ("X", 1)
        , ("Y", 2)
        , ("Z", 1)
        , ("_", 2)
        , ("Ñ", 3)
        ]
  

  case letterBagResult of
    Left err -> H.assertFailure $ "makeBag returned an error " ++ show err
    Right letterBag -> do
      assertEqual "Expected 28 different valid letters in the bag" (length (validLetters letterBag)) 28

      let takeResult = takeLetters letterBag 108

      case takeResult of
        Nothing -> H.assertFailure $ "takeLetters returned Nothing, Expected Just"
        Just (tiles, newBag) -> do
          assertEqual "Expected 103 tiles to be taken from the bag" (length tiles) 108
          assertEqual "Expect no letters to left in the bag" (bagSize newBag) 0
          let actualTileQuantities = M.toList (countLetters tiles)
          assertEqual "Expected the tiles" expectedTileQuantities actualTileQuantities

