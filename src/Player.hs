module Player (Player, LetterRack, name, rack, score, makePlayer, increaseScore, giveTiles, tilesOnRack,
 removePlayedTiles, removeTiles, hasEmptyRack, tileValues, reduceScore, exchange) where

  import Tile
  import Data.List
  import Data.Maybe
  import qualified Data.Map as Map

  type Score = Int
  type Name = String

  data LetterRack = LetterRack [Tile] deriving (Show, Eq)

  data Player = Player {name :: Name
                       , rack :: LetterRack
                       , score :: Score } deriving Show

  makePlayer :: String -> Player
  makePlayer playerName = Player playerName (LetterRack []) 0

  tilesOnRack :: Player -> [Tile]
  tilesOnRack (Player _ (LetterRack letters) _) = letters

  increaseScore :: Player -> Int -> Player
  increaseScore player justScored = player {score = currentScore + justScored}
    where
      currentScore = score player

  reduceScore :: Player -> Int -> Player
  reduceScore player removeScore = player {score = currentScore - removeScore}
    where
      currentScore = score player

  hasEmptyRack :: Player -> Bool
  hasEmptyRack player = null $ tilesOnRack player

  tileValues :: Player -> Int
  tileValues player = sum $ map tileValue (tilesOnRack player)

  {-
    Adds tiles to the player's tile rack.
  -}
  giveTiles :: Player -> [Tile] -> Player
  giveTiles player newTiles = player {rack = LetterRack $ newTiles ++ tilesOnRack player}

  removeTiles :: Player -> [Tile] -> Player
  removeTiles player toRemove = player {rack = LetterRack $ tilesOnRack player \\ toRemove}

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
      removedFromRack playing playedTiles = player {rack = LetterRack (deleteFirstsBy isPlayable (tilesOnRack playing) playedTiles) }

  {-
    Returns true if the player cannot place any of the given tiles. A player cannot play
    a Blank tile that they have not given a letter, or a tile not on their rack.
  -}
  playerCanPlace :: Player -> [Tile] -> Bool
  playerCanPlace player played = isNothing $ find isInvalid playedList
    where
      (playedFrequencies, rackFrequencies) = tileFrequencies played $ tilesOnRack player
      playedList = Map.toList playedFrequencies

      isInvalid (tile, freq) =
       case tile of
        -- Tried to play a blank without a letter
        Blank Nothing -> False 
        -- Player doesn't have tiles
        Blank _ -> freq > Map.findWithDefault 0 (Blank Nothing) rackFrequencies
        Letter chr val -> freq > Map.findWithDefault 0 (Letter chr val) rackFrequencies

  exchange :: Player -> [Tile] -> [Tile] -> Maybe Player
  exchange player exchanged received = 
    if not (playerCanExchange player exchanged) then Nothing
      else 
        Just $ giveTiles (removeTiles player exchanged) received

  playerCanExchange :: Player -> [Tile] -> Bool
  playerCanExchange (Player _ ( LetterRack letterRack) _ ) exchanged = isNothing $ find cannotExchange exchangedList

    where
      (exchangedFrequencies, rackFrequencies) = tileFrequencies exchanged letterRack
      exchangedList = Map.toList exchangedFrequencies

      cannotExchange (tile, freq) =
       case tile of
        -- Tried to exchange a blank letter which has been labeled. Client error.
        Blank (Just _) -> False 
        -- Player doesn't have tiles
        Blank _ -> freq > Map.findWithDefault 0 (Blank Nothing) rackFrequencies
        Letter chr val -> freq > Map.findWithDefault 0 (Letter chr val) rackFrequencies

  tileFrequencies :: [Tile] -> [Tile] -> ((Map.Map Tile Int), (Map.Map Tile Int))
  tileFrequencies given letterRack = (givenFrequencies, rackFrequencies)
    where
      buildFrequencies tiles = foldl addFrequency (Map.empty) tiles
      addFrequency dict tile = Map.alter newFrequency tile dict
      newFrequency m = Just $ maybe 1 succ m -- Default freq of one, or inc existing frequency
      givenFrequencies = buildFrequencies given
      rackFrequencies = buildFrequencies letterRack
