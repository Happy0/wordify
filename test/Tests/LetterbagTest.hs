module Tests.LetterBagTest where

    import Test.QuickCheck (Arbitrary, arbitrary, elements, Property, quickCheck, listOf, (==>), sized, oneof, choose, Gen)
    import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
    import LetterBag
    import Data.List
    import LetterBag.Internal
    import Tile

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
    
    shuffleProperty :: LetterBag -> Property
    shuffleProperty bag = monadicIO $
     do
      shuffled <- run $ shuffleBag bag
      assert $ if (bagSize bag < 10) then sameTiles bag shuffled else bagIsShuffled bag shuffled && sameTiles bag shuffled

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

        assert $
            case exchangeResult of
                Nothing -> originalNumTiles == 0
                Just (given, LetterBag newTiles newNumTiles) ->
                 (originalNumTiles == newNumTiles) && length given == length toExchange -- Todo: Finish this test function

            where
                LetterBag originalTiles originalNumTiles = letterBag

