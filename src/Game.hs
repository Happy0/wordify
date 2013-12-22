module Game(makeGame) where

  import Player
  import Board
  import Dictionary
  import LetterBag
  import Data.IntMap as IntMap

  type Turn = Int
  type MovesMade = Int

  data Game = Game (IntMap Player) Board LetterBag Dictionary Turn MovesMade


  makeGame :: [Player] -> Board -> LetterBag -> Dictionary -> Maybe Game
  makeGame players board bag dictionary = if (incorrectNumPlayers players)
   then Nothing else Just $ Game playerMap board bag dictionary 1 0
    where
      playerMap = IntMap.fromList $ zip [1 .. ] players
      incorrectNumPlayers (a:b:c:d:xs) = True -- > 4 players
      incorrectNumPlayers (a:[]) = True -- 1 Player
      incorrectNumPlayers _ = False -- 2 - 4 players

