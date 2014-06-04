module Wordify.Rules.Game(Game, player1, player2, optionalPlayers, currentPlayer,
 board, bag, dictionary, playerNumber, moveNumber, makeGame, getGameStatus,
  GameStatus(InProgress, Finished), gameStatus, players, passes, numberOfPlayers, history, History(History),movesMade) where

  import Wordify.Rules.Player
  import Wordify.Rules.Board
  import Wordify.Rules.Dictionary
  import Wordify.Rules.LetterBag
  import Data.IntMap as IntMap
  import Data.List
  import Wordify.Rules.ScrabbleError
  import Data.Maybe
  import Control.Applicative
  import Wordify.Rules.Game.Internal
  import qualified Data.Sequence as Seq
  import qualified Data.Foldable as F 
 
  {- |
    Starts a new game. 

    A game has at least 2 players, and 2 optional players (player 3 and 4.) The players should be newly created,
    as tiles from the letter bag will be distributed to each player.

    Takes a letter bag and dictionary, which should be localised to the same language.

    Yields a tuple with the first player and the initial game state. Returns a 'Left' if there are not enough
    tiles in the letter bag to distribute to the players.
  -}
  makeGame :: (Player, Player, Maybe (Player, Maybe Player)) -> LetterBag -> Dictionary -> Either ScrabbleError Game
  makeGame (play1, play2, optionalPlayers) bag dictionary =
   if (numberOfPlayers * 7 > lettersInBag) then Left (NotEnoughLettersInStartingBag lettersInBag)
    else Right $ (Game player1 player2 optional emptyBoard finalBag dictionary player1 1 1 0 InProgress initialHistory)
    where
          lettersInBag = bagSize bag
          numberOfPlayers = 2 + maybe 0 (\(player3, maybePlayer4) -> if isJust maybePlayer4 then 2 else 1) optionalPlayers
          (player1, firstBag) = givePlayerTiles play1 bag
          (player2, secondBag) = givePlayerTiles play2 firstBag
          (optional, finalBag) = maybe (Nothing, secondBag) (\optional -> fillOptional optional secondBag) optionalPlayers
          givePlayerTiles player bag = maybe (player, bag) (\(tiles, newBag) -> (giveTiles player tiles, newBag) ) $ takeLetters bag 7

          fillOptional (thirdPlayer, optional) bag =
             case optional of 
                Nothing -> (Just (player3, Nothing), thirdBag)
                Just (lastPlayer) -> let (player4, fourthBag) = makePlayer4 lastPlayer
                                     in (Just (player3, (Just player4)), fourthBag)
             where
                (player3, thirdBag) = givePlayerTiles thirdPlayer bag
                makePlayer4 player = givePlayerTiles player thirdBag

          initialHistory = History bag Seq.empty

  players :: Game -> [Player]
  players game = [player1 game, player2 game] ++ optionals
   where
      optionals = case (optionalPlayers game) of 
                    Nothing -> []
                    Just (player3, Nothing) -> [player3]
                    Just (player3, Just player4) -> [player3, player4]

  getGameStatus :: Game -> GameStatus
  getGameStatus game = gameStatus game

  numberOfPlayers :: Game -> Int
  numberOfPlayers game = 2 + maybe 0 (\(player3, maybePlayer4) -> if isJust maybePlayer4 then 2 else 1) (optionalPlayers game)

  {- |
    Returns a history of the moves made in the game.
  -}
  movesMade :: Game -> [Move]
  movesMade game = 
    case (history game) of 
      History _ moves -> F.toList moves
