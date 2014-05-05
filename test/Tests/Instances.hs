module Tests.Instances where

    import Tile
    import Test.QuickCheck (Arbitrary, arbitrary, listOf, (==>), sized, oneof, choose, Gen, elements)
    import LetterBag
    import Pos
    import Data.Char
    import Pos.Internal
    import Square
    import Data.Map
    import Board
    import Board.Internal
    import System.Random
    import LetterBag.Internal

    instance Arbitrary Tile where
        arbitrary = do
            chr <- arbitrary :: Gen Char
            value <- arbitrary :: Gen Int
            tile <- elements [Letter chr value, Blank Nothing]
            return tile

    instance Arbitrary LetterBag where
        arbitrary = do
           tiles <- listOf (arbitrary :: Gen Tile)
           seed <- arbitrary :: Gen Int
           let generator = mkStdGen seed
           return $ LetterBag tiles (length tiles) generator

    instance Arbitrary Pos where
        arbitrary = do
           x <- choose (1,15)
           y <- choose (1,15)
           let gridCo = [chr (x + 64)] ++ (show y)
           return $ Pos x y gridCo

    instance Arbitrary Square where
        arbitrary = do
            tile <- arbitrary :: Gen Tile
            square <- elements [Normal (Just tile), Normal Nothing, DoubleLetter (Just tile), DoubleLetter Nothing, DoubleWord (Just tile),
             DoubleWord Nothing, TripleLetter (Just tile), TripleLetter Nothing, TripleWord (Just tile), TripleWord Nothing]
            return square

    instance Arbitrary Board where
        arbitrary = do
            let Board squares = emptyBoard
            let originalSquares = toList squares
            positions <- listOf (arbitrary :: Gen Pos)
            squares <- listOf (arbitrary :: Gen Square)
            return $ Board $ fromList $ originalSquares ++ (zip positions squares)
