module Player (Player, LetterRack, rack, score, makePlayer, updateScore, giveTiles,
 removePlayedTiles, hasEmptyRack, tileValues, reduceScore, playerCanExchange) where

  import Tile
  import Data.List
  import Data.Maybe
  import qualified Data.Map as Map

  type Score = Int
  type Name = String

  data LetterRack = LetterRack [Tile] deriving Show

  data Player = Player {name :: Name
                       ,rack :: LetterRack
                       , score :: Score } deriving Show

  makePlayer :: String -> Player
  makePlayer name = Player name (LetterRack []) 0

  updateScore :: Player -> Int -> Player
  updateScore player justScored = player {score = currentScore + justScored}
    where
      currentScore = score player

  reduceScore :: Player -> Int -> Player
  reduceScore player removeScore = player {score = currentScore - removeScore}
    where
      currentScore = score player

  hasEmptyRack :: Player -> Bool
  hasEmptyRack (Player name (LetterRack rack) score) = null rack

  tileValues :: Player -> Int
  tileValues (Player name (LetterRack (rack)) score) = sum $ map tileValue rack

  {-
    Adds tiles to the player's tile rack.
  -}
  giveTiles :: Player -> [Tile] -> Player
  giveTiles (Player name (LetterRack tiles) score) newTiles =
   Player name (LetterRack $ newTiles ++ tiles) score

  {-
    Removes played tiles from the player's tile rack, if it was possible for the player
    to play those tiles in the first place. A player may play a tile on his rack, unless
    it is a blank, which must first be assigned a letter. 
  -}
  removePlayedTiles :: Player -> [Tile] -> Maybe Player
  removePlayedTiles player tiles =
    if (playerCanPlace player tiles)
     then Just $  player `removedFromRack` tiles
      else Nothing
    where
      removedFromRack (Player name (LetterRack rack) score) tiles = 
        Player name (LetterRack $ deleteFirstsBy isPlayable rack tiles) score

  {-
    Returns true if the player cannot place any of the given tiles. A player cannot play
    a Blank tile that they have not given a letter, or a tile not on their rack.
  -}
  playerCanPlace :: Player -> [Tile] -> Bool
  playerCanPlace (Player _ (LetterRack rack) _ ) played = isNothing $ find isInvalid playedList
    where
      (playedFrequencies, rackFrequencies) = tileFrequencies played rack
      playedList = Map.toList playedFrequencies

      isInvalid (tile, freq) =
       case tile of
        -- Tried to play a blank without a letter
        Blank Nothing -> False 
        -- Player doesn't have tiles
        Blank _ -> freq > Map.findWithDefault 0 (Blank Nothing) rackFrequencies
        Letter chr val -> freq > Map.findWithDefault 0 (Letter chr val) rackFrequencies

  playerCanExchange :: Player -> [Tile] -> Bool
  playerCanExchange (Player _ ( LetterRack rack) _ ) exchanged = isNothing $ find cannotExchange exchangedList

    where
      (exchangedFrequencies, rackFrequencies) = tileFrequencies exchanged rack
      exchangedList = Map.toList exchangedFrequencies

      cannotExchange (tile, freq) =
       case tile of
        -- Tried to exchange a blank letter which has been labeled. Client error.
        Blank (Just _) -> False 
        -- Player doesn't have tiles
        Blank _ -> freq > Map.findWithDefault 0 (Blank Nothing) rackFrequencies
        Letter chr val -> freq > Map.findWithDefault 0 (Letter chr val) rackFrequencies

  tileFrequencies :: [Tile] -> [Tile] -> ((Map.Map Tile Int), (Map.Map Tile Int))
  tileFrequencies given rack = (givenFrequencies, rackFrequencies)
    where
      buildFrequencies tiles = foldl addFrequency (Map.empty) tiles
      addFrequency dict tile = Map.alter newFrequency tile dict
      newFrequency m = Just $ maybe 1 succ m -- Default freq of one, or inc existing frequency
      givenFrequencies = buildFrequencies given
      rackFrequencies = buildFrequencies rack