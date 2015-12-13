module Wordify.Rules.Game(Game,
                          makeGame,
                          movesMade,
                          gameStatus,
                          board,
                          bag,
                          dictionary,
                          moveNumber,
                          player1,
                          player2,
                          optionalPlayers,
                          currentPlayer,
                          getPlayer,
                          playerNumber,
                          GameStatus(InProgress, Finished),
                          players,
                          passes,
                          numberOfPlayers,
                          history,
                          History(History)) where

  import Wordify.Rules.Player
  import Wordify.Rules.Board
  import Wordify.Rules.Dictionary
  import Wordify.Rules.LetterBag
  import Wordify.Rules.ScrabbleError
  import Data.Maybe
  import Wordify.Rules.Game.Internal
  import qualified Data.Sequence as Seq
  import qualified Data.Foldable as F
  import Safe
  import Control.Monad
  import Control.Applicative
  import qualified Data.List.Safe as LS

  {- |
    Starts a new game. 

    A game has at least 2 players, and 2 optional players (player 3 and 4.) The players should be newly created,
    as tiles from the letter bag will be distributed to each player.

    Takes a letter bag and dictionary, which should be localised to the same language.

    Yields a tuple with the first player and the initial game state. Returns a 'Left' if there are not enough
    tiles in the letter bag to distribute to the players.
  -}
  makeGame :: (Player, Player, Maybe (Player, Maybe Player)) -> LetterBag -> Dictionary -> Either ScrabbleError Game
  makeGame playing startBag dict =
    do
      (remainingBag, initialPlayers) <- initialisePlayers playing startBag
      case initialPlayers of
        firstPlayer : secondPlayer : optionals ->
         return $ Game firstPlayer secondPlayer (toOptionalPlayers optionals) emptyBoard
          remainingBag dict firstPlayer firstPlayerNumber firstMoveNumber initialPasses initialGameState initialHistory
        _ -> Left $ MiscError "Unexpected error in logic. There were not at least two players. This should never happen."
      where
        initialHistory = History startBag Seq.empty
        firstPlayerNumber = 1
        firstMoveNumber = 1
        initialPasses = 0
        initialGameState = InProgress

        toOptionalPlayers :: [Player] -> Maybe (Player, Maybe Player)
        toOptionalPlayers (x : []) = Just (x, Nothing)
        toOptionalPlayers (x : y : []) = Just (x, Just y)
        toOptionalPlayers _ = Nothing


  initialisePlayers :: (Player, Player, Maybe (Player, Maybe Player)) -> LetterBag -> Either ScrabbleError (LetterBag, [Player])
  initialisePlayers (play1, play2, maybePlayers) initialBag = 
    let transitions = distributeFinite givePlayerTiles initialBag allPlayers
    in case (join . lastMay) transitions of
      Nothing -> Left $ NotEnoughLettersInStartingBag (bagSize initialBag)
      Just (finalBag, _) -> Right (finalBag, map snd (catMaybes transitions))
    where
      allPlayers = [play1, play2] ++ optionalsToPlayers maybePlayers
      givePlayerTiles currentBag giveTo = 
        (\(newTiles, newBag) -> (newBag, giveTiles giveTo newTiles)) <$> takeLetters currentBag 7

      distributeFinite :: (b -> a -> Maybe (b, a)) -> b -> [a] -> [Maybe (b, a)]
      distributeFinite takeUsing distributeFrom = go takeUsing (Just distributeFrom)
        where
          go :: (b -> a -> Maybe (b,a)) -> Maybe b -> [a] -> [Maybe (b, a)]
          go _ _ [] = []
          go giveBy Nothing (_ : receivers) = Nothing : go giveBy Nothing receivers
          go giveBy (Just distributer) (receiver : receivers) =
            case giveBy distributer receiver of
              Nothing -> Nothing : go giveBy Nothing receivers
              Just (currentDistributer, currentReceiver) -> 
                Just (currentDistributer, currentReceiver) : go giveBy (Just currentDistributer) receivers


  optionalsToPlayers :: Maybe (Player, Maybe Player) -> [Player]
  optionalsToPlayers optionals = case optionals of 
                    Nothing -> []
                    Just (player3, Nothing) -> [player3]
                    Just (player3, Just player4) -> [player3, player4]


  players :: Game -> [Player]
  players game = [player1 game, player2 game] ++ optionalsToPlayers (optionalPlayers game)

  getPlayer :: Game -> Int -> Maybe Player
  getPlayer game playerNumber = (players game) LS.!! (playerNumber - 1)

  numberOfPlayers :: Game -> Int
  numberOfPlayers game = 2 + maybe 0 (\(_, maybePlayer4) -> if isJust maybePlayer4 then 2 else 1) (optionalPlayers game)

  {- |
    Returns a history of the moves made in the game.
  -}
  movesMade :: Game -> [Move]
  movesMade game = 
    case history game of 
      History _ moves -> F.toList moves
