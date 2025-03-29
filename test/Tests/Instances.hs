module Tests.Instances where

import Data.Char
import Data.Map
import System.Random
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, elements, listOf, oneof, sized, (==>))
import Wordify.Rules.Board
import Wordify.Rules.Board.Internal
import Wordify.Rules.LetterBag
import Wordify.Rules.LetterBag.Internal
import Wordify.Rules.Pos
import Wordify.Rules.Pos.Internal
import Wordify.Rules.Square
import Wordify.Rules.Tile

instance Arbitrary Tile where
  arbitrary = do
    chr <- arbitrary :: Gen String
    value <- arbitrary :: Gen Int
    tile <- elements [Letter chr value, Blank Nothing]
    return tile

instance Arbitrary LetterBag where
  arbitrary = do
    tiles <- listOf (arbitrary :: Gen Tile)
    seed <- arbitrary :: Gen Int
    let generator = mkStdGen seed
    return $ makeBagUsingGenerator tiles generator

instance Arbitrary Pos where
  arbitrary = do
    x <- choose (1, 15)
    y <- choose (1, 15)
    let gridCo = [chr (x + 64)] ++ (show y)
    return $ Pos x y gridCo

instance Arbitrary Square where
  arbitrary = do
    tile <- arbitrary :: Gen Tile
    square <-
      elements
        [ Normal (Just tile),
          Normal Nothing,
          DoubleLetter (Just tile),
          DoubleLetter Nothing,
          DoubleWord (Just tile),
          DoubleWord Nothing,
          TripleLetter (Just tile),
          TripleLetter Nothing,
          TripleWord (Just tile),
          TripleWord Nothing
        ]
    return square

instance Arbitrary Board where
  arbitrary = do
    let Board squares = emptyBoard
    let originalSquares = toList squares
    positions <- listOf (arbitrary :: Gen Pos)
    squares <- listOf (arbitrary :: Gen Square)
    return $ Board $ fromList $ originalSquares ++ (zip positions squares)
