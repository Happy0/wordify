module Game(makeGame) where

  import Player
  import Board
  import Dictionary
  import LetterBag
  import Data.IntMap as IntMap
  import Data.List

  type PlayerNumber = Int
  type MovesMade = Int

  data Game = Game { players :: IntMap Player
                     , board :: Board
                     , bag :: LetterBag
                     , dictionary :: Dictionary 
                     , playerNumber :: Int
                     , moveNumber :: Int }

  makeGame :: [String] -> Board -> LetterBag -> Dictionary -> Maybe (Player, Game)
  makeGame [] _ _ _ = Nothing
  makeGame [x] _ _ _ = Nothing
  makeGame (a:b:c:d:e:xs) _ _ _ = Nothing 
  makeGame names board bag dictionary = Just $ (player1, Game playerMap board startBag dictionary 0 1)
    where
      playerMap = IntMap.fromList $ zip [0 .. ] (player1 : playersWithRacks )
      (startBag, player1 : playersWithRacks) = mapAccumL givePlayerFromBag bag initPlayers
      initPlayers = Prelude.map makePlayer names
      givePlayerFromBag bag player = -- Assume sensible letter bag input (big enough to distribute tiles)
        maybe ((bag, player)) (\(tiles, bag) -> (bag, giveTiles player tiles)) $ takeLetters bag 7

  nextPlayerNumber :: Game -> Int
  nextPlayerNumber game = (succ $ playerNumber game) `mod` (size $ players game)

  updateGame :: Game -> Player -> Board -> LetterBag -> Maybe (Player, Game)
  updateGame game player newBoard newBag = fmap (\nextPlayer -> (nextPlayer, newGame) ) $ IntMap.lookup nextPlayer playerMap
    where
      newGame = Game (IntMap.insert currentPlayer player playerMap) newBoard newBag dict nextPlayer move
      currentPlayer = playerNumber game
      playerMap = players game
      nextPlayer = nextPlayerNumber game
      move = succ $ moveNumber game
      dict = dictionary game