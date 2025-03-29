module Tests.LetterBagTest where

import qualified Data.List as L
import Data.Map
import Data.Maybe
import System.IO (hClose, hFlush, hPutStr, hPutStrLn)
import Test.HUnit.Base as H
import Test.QuickCheck (Property, quickCheck)
import Test.QuickCheck.Monadic as Q (assert, monadicIO, pick, pre, run)
import Tests.Instances
import Tests.Utils
import Wordify.Rules.LetterBag
import Wordify.Rules.LetterBag.Internal
import Wordify.Rules.ScrabbleError
import Wordify.Rules.Tile

bagFromTilesProperty :: [Tile] -> Property
bagFromTilesProperty inputTiles = monadicIO $
  do
    bag <- run $ bagFromTiles inputTiles
    let LetterBag resultingTiles numTiles generator validLetters = bag
    Q.assert $ numTiles == (length inputTiles) && resultingTiles == inputTiles

shuffleProperty :: LetterBag -> Bool
shuffleProperty bag =
  let shuffled = shuffleBag bag
   in if (bagSize bag < 10) then sameTiles bag shuffled else bagIsShuffled bag shuffled && sameTiles bag shuffled
  where
    sameTiles originalBag shuffledBag = bagSize originalBag == (length $ (tiles originalBag) `L.intersect` (tiles shuffledBag))
    bagIsShuffled originalBag shuffledBag = not $ originalBag == shuffledBag

shuffleTwiceProperty :: LetterBag -> Bool
shuffleTwiceProperty bag = if (bagSize bag < 10) then True else not $ bag1 == bag2 && bagSize bag1 == bagSize bag2
  where
    bag1 = shuffleBag bag
    bag2 = shuffleBag bag1

takeLettersProperty :: LetterBag -> Int -> Bool
takeLettersProperty letterBag numTake =
  if (originalBagSize < numTake)
    then takeLetters letterBag numTake == Nothing
    else takeLetters letterBag numTake == Just (expectedTiles, expectedBag)
  where
    LetterBag originalBagTiles originalBagSize gen validLetters = letterBag
    expectedTiles = L.take numTake originalBagTiles
    expectedBag = LetterBag (L.drop numTake originalBagTiles) (originalBagSize - numTake) gen validLetters

exchangeLettersProperty :: LetterBag -> [Tile] -> Bool
exchangeLettersProperty letterBag toExchange =
  let exchangeResult = exchangeLetters letterBag toExchange
   in case exchangeResult of
        Nothing -> originalNumTiles == 0
        Just (given, LetterBag newTiles newNumTiles newGenerator validLetters) ->
          (originalNumTiles == newNumTiles)
            && length given == length toExchange
            && forAll (\tile -> (getCount tile newTileCounts) == (getCount tile originalTileCounts) + (getCount tile exchangedCounts) - (getCount tile givenCounts)) allTiles
          where
            allTiles = given ++ newTiles ++ originalTiles
            givenCounts = countMap given
            newTileCounts = countMap newTiles
            originalTileCounts = countMap originalTiles
            exchangedCounts = countMap toExchange
            countMap xs = fromListWith (+) [(x, 1) | x <- xs]
            getCount key m = findWithDefault 0 key m

            forAll condition list = L.null $ L.filter (not . condition) list
  where
    LetterBag originalTiles originalNumTiles generator validLetters = letterBag

makeBagInvalidlyFormattedBag :: Assertion
makeBagInvalidlyFormattedBag =
  withTempFile $ \filePath handle -> do
    let invalidStr = "A 2 2 3 4" -- Erroneous extra number
    hPutStrLn handle invalidStr
    hFlush handle
    hClose handle
    letterBag <- makeBag filePath

    case letterBag of
      Left (MalformedLetterBagFile _) -> return ()
      x -> H.assertFailure $ "Input with invalidly formatted bag unexpectedly succeeded: " ++ show x

makeBagTestSuccess :: Assertion
makeBagTestSuccess =
  withTempFile $ \filePath handle -> do
    let letters = L.map (\x -> [x]) ['A' ..]
    let values = [1 .. 5]
    let distributions = [1 .. 5]
    let inputLines = unlines $ zipWith3 (\tileLetters value distribution -> L.intersperse ' ' $ tileLetters ++ (show value) ++ (show distribution)) letters values distributions
    hPutStrLn handle "_ 2" -- 2 Blank tiles
    hPutStr handle inputLines
    hFlush handle
    hClose handle
    letterBag <- makeBag filePath

    case letterBag of
      Left _ -> H.assertFailure "makeBag returned an error"
      Right (LetterBag tiles numTiles generator validLetters) -> do
        let expectedLetters = zipWith3 (\letter value distribution -> replicate distribution $ Letter letter value) letters values distributions
        let expectedBlanks = replicate 2 $ Blank Nothing
        let expectedTiles = concat $ expectedBlanks : expectedLetters

        H.assertBool "Letter bag contains expected letters" $ expectedTiles `L.intersect` tiles == expectedTiles
        H.assertBool "Letter bag contains expected number of letters" $ (length expectedTiles) == (length tiles)

makeBagInvalidPath :: Assertion
makeBagInvalidPath =
  do
    letterBag <- makeBag "this is an invalid file path"
    case letterBag of
      Left (LetterBagFileNotOpenable _) -> return ()
      _ -> H.assertFailure "Unexpected success"

    return ()
