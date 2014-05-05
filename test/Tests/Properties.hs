module Tests.Properties
    (
      tests
    ) where


  import Test.Framework (Test, testGroup)
  import Test.Framework.Providers.QuickCheck2 (testProperty)
  import Tests.PosTest
  import Tests.LetterBagTest
  import Tests.BoardTest

  tests :: Test
  tests =  testGroup "Properties" [
   testGroup "Positions" [
    testProperty "Out of bounds positions return Nothing" inBoundsProperty,
    testProperty "Gives correct position to right if in bounds" lettersRightProperty,
    testProperty "Give correct position to left if in bounds" lettersLeftProperty,
    testProperty "Give correct position above if in bounds" lettersAboveProperty,
    testProperty "Give correct position below if in bounds" lettersBelowProperty,
    testProperty "Gives correct grid co-ordinates for positions" correctGridPos],

    testGroup "LetterBag" [
      testProperty "Bag is constructed from list of tiles correctly" bagFromTilesProperty,
      testProperty "Bag contents are shuffled correctly" shuffleProperty,
      testProperty "Shuffling a bag twice yields two different sets of tiles" shuffleTwiceProperty,
      testProperty "Taking tiles from the bag behaves correctly" takeLettersProperty,
      testProperty "Letter exchange behaves correctly" exchangeLettersProperty
    ],

    testGroup "Board" [
      testProperty "Place tile is only valid for empty squares on the board" placeTileProperty

    ]

    ]