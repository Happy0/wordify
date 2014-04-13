module Tests.LetterBagTest where

    import Test.QuickCheck (Arbitrary, arbitrary, elements, Property, quickCheck, listOf, (==>), sized, oneof, choose, Gen)
    import Test.QuickCheck.Monadic as Q (assert, monadicIO, pick, pre, run) 
    import LetterBag
    import Data.List
    import LetterBag.Internal
    import Tile
    import Tests.Utils
    import System.IO (hPutStr, hFlush, hPutStrLn, hClose)
    import Test.HUnit.Base as H
    import ScrabbleError
    import Data.Map
    import Data.Maybe
    import qualified Data.List as L

    instance Arbitrary Tile where
        arbitrary = do
            chr <- arbitrary :: Gen Char
            value <- arbitrary :: Gen Int
            tile <- elements [Letter chr value, Blank Nothing]
            return tile

    instance Arbitrary LetterBag where
        arbitrary = do
           tiles <- listOf (arbitrary :: Gen Tile)
           return $ bagFromTiles (tiles)

    bagFromTilesProperty :: [Tile] -> Bool
    bagFromTilesProperty inputTiles = numTiles == (length inputTiles) && resultingTiles == inputTiles
      where
        LetterBag resultingTiles numTiles = bagFromTiles inputTiles
    
    shuffleProperty :: LetterBag -> Property
    shuffleProperty bag = monadicIO $
     do
      shuffled <- run $ shuffleBag bag
      Q.assert $ if (bagSize bag < 10) then sameTiles bag shuffled else bagIsShuffled bag shuffled && sameTiles bag shuffled

      where
        sameTiles originalBag shuffledBag = bagSize originalBag == (length $ (tiles originalBag) `intersect` (tiles shuffledBag))
        bagIsShuffled originalBag shuffledBag = not $ originalBag == shuffledBag

    takeLettersProperty :: LetterBag -> Int -> Bool
    takeLettersProperty letterBag numTake = 
        if (originalBagSize < numTake) then takeLetters letterBag numTake == Nothing
         else
          takeLetters letterBag numTake == Just (expectedTiles, expectedBag)
        where
            LetterBag originalBagTiles originalBagSize = letterBag
            expectedTiles = take numTake originalBagTiles
            expectedBag = LetterBag (drop numTake originalBagTiles) (originalBagSize - numTake)

    exchangeLettersProperty :: LetterBag -> [Tile] -> Property
    exchangeLettersProperty letterBag toExchange = monadicIO $
      do 
        exchangeResult <- run $ exchangeLetters letterBag toExchange

        Q.assert $
            case exchangeResult of
                Nothing -> originalNumTiles == 0
                Just (given, LetterBag newTiles newNumTiles) ->
                 (originalNumTiles == newNumTiles)
                  && length given == length toExchange
                   && forAll (\tile -> (getCount tile newTileCounts) == (getCount tile originalTileCounts) + (getCount tile exchangedCounts) - (getCount tile givenCounts) ) allTiles

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
                LetterBag originalTiles originalNumTiles = letterBag

    makeBagInvalidlyFormattedBag :: IO ()
    makeBagInvalidlyFormattedBag = 
      withTempFile $ \ filePath handle -> do
        let invalidStr = "A 2 2 3 4" -- Erroneous extra number
        hPutStrLn handle invalidStr
        hFlush handle
        hClose handle
        letterBag <- makeBag filePath

        case letterBag of 
          Left (MalformedLetterBagFile _) -> return ()
          x -> H.assertFailure $ "Input with invalidly formatted bag unexpectedly succeeded: " ++ show x

    makeBagTestSuccess :: IO ()
    makeBagTestSuccess = 
        withTempFile $ \ filePath handle -> do
          let letters = ['A' .. ]
          let values = [1 .. 5]
          let distributions = [1 .. 5]
          let inputLines = unlines $ zipWith3 (\letter value distribution -> intersperse ' ' $ letter : (show value) ++ (show distribution)) letters values distributions
          hPutStrLn handle "_ 2" -- 2 Blank tiles
          hPutStr handle inputLines
          hFlush handle
          hClose handle
          letterBag <- makeBag filePath
          
          case letterBag of 
            Left _ -> H.assertFailure "makeBag returned an error"
            Right (LetterBag tiles numTiles) -> do
              let expectedLetters = zipWith3 (\letter value distribution -> replicate distribution $ Letter letter value  ) letters values distributions
              let expectedBlanks = replicate 2 $ Blank Nothing
              let expectedTiles = concat $ expectedBlanks : expectedLetters 

              H.assertBool "Letter bag contains expected letters" $ expectedTiles `intersect` tiles == expectedTiles
              H.assertBool "Letter bag contains expected number of letters" $ (length expectedTiles) == (length tiles)

    makeBagInvalidPath :: IO ()
    makeBagInvalidPath =
     do
      letterBag <- makeBag "this is an invalid file path" 
      case letterBag of 
        Left (LetterBagFileNotOpenable _) -> return ()
        _ -> H.assertFailure "Unexpected success"

      return ()


