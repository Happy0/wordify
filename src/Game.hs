module Game(makeGame) where

  import Player
  import Board
  import Dictionary
  import LetterBag
  import Data.IntMap as IntMap
  import Data.List
  import ScrabbleError
  import Control.Applicative

  data Game = Game { players :: IntMap Player
                     , board :: Board
                     , bag :: LetterBag
                     , dictionary :: Dictionary 
                     , playerNumber :: Int
                     , moveNumber :: Int }

  makeGame :: [String] -> Board -> LetterBag -> Dictionary -> Either ScrabbleError (Player, Game)
  makeGame [] _ _ _ = Left $ InvalidNumberOfPlayers
  makeGame [x] _ _ _ = Left $ InvalidNumberOfPlayers
  makeGame (a:b:c:d:e:xs) _ _ _ = Left $ InvalidNumberOfPlayers
  makeGame names board bag dictionary = if ( numTiles < numPlayers * 7) then Left (NotEnoughLettersInStartingBag numTiles)
   else Right $ (player1, Game playerMap board startBag dictionary 0 1)
    where
      playerMap = IntMap.fromList $ zip [0 .. ] (player1 : playersWithRacks )
      (startBag, player1 : playersWithRacks) = mapAccumL givePlayerFromBag bag initPlayers
      initPlayers = Prelude.map makePlayer names
      givePlayerFromBag bag player =
       -- We have already checked there are enough tiles to distribute, use 'maybe' to appease compiler
        maybe ((bag, player)) (\(tiles, bag) -> (bag, giveTiles player tiles)) $ takeLetters bag 7

      numPlayers = length $ names
      numTiles = (bagSize bag)

  nextPlayerNumber :: Game -> Int
  nextPlayerNumber game = (succ $ playerNumber game) `mod` (size $ players game)

  updateGame :: Game -> Player -> Board -> LetterBag -> Maybe (Player, Game)
  updateGame game player newBoard newBag = (\nextPlayer -> (nextPlayer, newGame) ) <$> IntMap.lookup nextPlayer playerMap
    where
      newGame = Game (IntMap.insert currentPlayer player playerMap) newBoard newBag dict nextPlayer move
      currentPlayer = playerNumber game
      playerMap = players game
      nextPlayer = nextPlayerNumber game
      move = succ $ moveNumber game
      dict = dictionary game