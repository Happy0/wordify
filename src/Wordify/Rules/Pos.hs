
module Wordify.Rules.Pos (Pos,
                          posAt,
                          above,
                          below,
                          left,
                          right,
                          xPos,
                          yPos,
                          gridValue,
                          starPos,
                          posMin,
                          posMax) where

  import qualified Data.Map as Map
  import Wordify.Rules.Pos.Internal
  import Data.Maybe

  posMin :: Int
  posMin = 1 

  posMax :: Int
  posMax = 15 

  posAt :: (Int, Int) -> Maybe Pos
  posAt = flip Map.lookup posMap

  -- | The position above the given position, if it exists.
  above :: Pos -> Maybe Pos
  above (Pos x y _) = posAt (x,y + 1) 

  -- | The position below the given position, if it exists.
  below :: Pos -> Maybe Pos
  below (Pos x y _) = posAt (x, y - 1 ) 

  -- | The position to the left of the given position, if it exists.
  left :: Pos -> Maybe Pos
  left (Pos x y _) = posAt (x - 1, y) 

  -- | The position to the right of the given position, if it exists.
  right :: Pos -> Maybe Pos
  right (Pos x y _) = posAt (x + 1, y)

  -- | The position of the star square
  starPos :: Pos
  starPos = Pos 8 8 "H8"

  {- A map keyed by tuples representing (x,y) co-ordinates, and valued by their
  corresponding Pos types -}
  posMap :: Map.Map (Int, Int) Pos
  posMap = Map.fromList $ catMaybes coordTuples
      where
          coordTuples = zipWith makeTuple (sequence [[posMin..posMax], [posMin..posMax]]) $ cycle ['A'..'O']
          makeTuple (x:y:_) gridLetter = Just $ ((y,x) , Pos y x (gridLetter : show x) )
          makeTuple _ _ = Nothing        

  xPos :: Pos -> Int
  xPos (Pos x _ _) = x

  yPos :: Pos -> Int
  yPos (Pos _ y _) = y

  gridValue :: Pos -> String
  gridValue (Pos _ _ grid) = grid

