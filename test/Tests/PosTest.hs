module Tests.PosTest (inBoundsProperty, lettersRightProperty, lettersLeftProperty, lettersBelowProperty, lettersAboveProperty, correctGridPos) where

  import Pos
  import Data.Maybe
  import Test.QuickCheck.Arbitrary
  import Data.Char

  inBoundsProperty :: (Int, Int) -> Bool
  inBoundsProperty (x,y)
    | (x > 15) = isNothing $ posAt (x,y)
    | (y > 15) = isNothing $ posAt (x,y)
    | (x < 1) = isNothing $ posAt (x,y)
    | (y < 1) = isNothing $ posAt (x,y)
    | otherwise = isJust $ posAt (x,y)

  correctGridPos :: (Int, Int) -> Bool
  correctGridPos (x,y) 
    | (x >= 1 && x <= 15 && y >= 1 && y <= 15) = 
        let pos = posAt (x,y) in 
          if isNothing pos then False else maybe (False) (\thePos -> gridValue thePos == expectedGrid) pos

    | otherwise = isNothing $ posAt (x,y)

    where
      expectedGrid = [chr (x + 64)] ++ (show y)

  lettersRightProperty :: Pos -> Bool
  lettersRightProperty pos
    | (xPos pos == 15) = isNothing $ right pos
    | otherwise = let rightPos = right pos
        in maybe (False) (\rightPos -> xPos rightPos == xPos pos + 1 && yPos pos == yPos rightPos) rightPos

  lettersLeftProperty :: Pos -> Bool
  lettersLeftProperty pos
    | (xPos pos == 1) = isNothing $ left pos
    | otherwise = let leftPos = left pos
        in maybe (False) (\leftPos -> xPos leftPos == xPos pos - 1 && yPos pos == yPos leftPos) leftPos

  lettersBelowProperty :: Pos -> Bool
  lettersBelowProperty pos
    | (yPos pos == 1) = isNothing $ below pos
    | otherwise = let belowPos = below pos
        in maybe (False) (\belowPos -> yPos belowPos == yPos pos - 1 && xPos pos == xPos belowPos) belowPos

  lettersAboveProperty :: Pos -> Bool
  lettersAboveProperty pos
    | (yPos pos == 15) = isNothing $ above pos
    | otherwise = let abovePos = above pos
        in maybe (False) (\abovePos -> yPos abovePos == yPos pos + 1 && xPos pos == xPos abovePos) abovePos
