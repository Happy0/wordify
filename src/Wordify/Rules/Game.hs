module Wordify.Rules.Game(Game, player1, player2, optionalPlayers, currentPlayer,
 board, bag, dictionary, playerNumber, moveNumber, makeGame, getGameStatus,
  GameStatus(InProgress, Finished), gameStatus, players, passes, numberOfPlayers, history, History(History),movesMade) where

  import Wordify.Rules.Player
  import Wordify.Rules.Board
  import Wordify.Rules.Dictionary
  import Wordify.Rules.LetterBag
  import Wordify.Rules.ScrabbleError
  import Data.Maybe
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
  makeGame (play1, play2, maybePlayers) initialBag dict =
   if (numPlayers * 7 > lettersInBag) then Left (NotEnoughLettersInStartingBag lettersInBag)
    else Right $ (Game initialPlayer1 initialPlayer2 initialOptionalPlayers emptyBoard finalBag dict initialPlayer1 1 1 0 InProgress initialHistory)
    where
          lettersInBag = bagSize initialBag
          numPlayers = 2 + maybe 0 (\(_, maybePlayer4) -> if isJust maybePlayer4 then 2 else 1) maybePlayers
          (initialPlayer1, firstBag) = givePlayerTiles play1 initialBag
          (initialPlayer2, secondBag) = givePlayerTiles play2 firstBag
          (initialOptionalPlayers, finalBag) = maybe (Nothing, secondBag) (\optional -> fillOptional optional secondBag) maybePlayers
          givePlayerTiles player currentBag = maybe (player, currentBag) (\(givenTiles, newBag) -> (giveTiles player givenTiles, newBag) ) $ takeLetters currentBag 7

          fillOptional (thirdPlayer, optional) letterBag =
             case optional of 
                Nothing -> (Just (player3, Nothing), thirdBag)
                Just (lastPlayer) -> let (player4, fourthBag) = makePlayer4 lastPlayer
                                     in (Just (player3, (Just player4)), fourthBag)
             where
                (player3, thirdBag) = givePlayerTiles thirdPlayer letterBag
                makePlayer4 player = givePlayerTiles player thirdBag

          initialHistory = History initialBag Seq.empty

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
  numberOfPlayers game = 2 + maybe 0 (\(_, maybePlayer4) -> if isJust maybePlayer4 then 2 else 1) (optionalPlayers game)

  {- |
    Returns a history of the moves made in the game.
  -}
  movesMade :: Game -> [Move]
  movesMade game = 
    case (history game) of 
      History _ moves -> F.toList moves
