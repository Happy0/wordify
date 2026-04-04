module Wordify.Rules.Game(Game,
                          makeGame,
                          restoreGame,
                          restoreGameLazy,
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
  import Wordify.Rules.WordifyError
  import Wordify.Rules.Move (makeMove, newGame, GameTransition)
  import Data.Maybe
  import Wordify.Rules.Game.Internal
  import qualified Data.List.NonEmpty as NE
  import qualified Data.Sequence as Seq
  import qualified Data.Traversable as T
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
  makeGame :: (Player, Player, Maybe (Player, Maybe Player)) -> LetterBag -> Dictionary -> Either WordifyError Game
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


  initialisePlayers :: (Player, Player, Maybe (Player, Maybe Player)) -> LetterBag -> Either WordifyError (LetterBag, [Player])
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


  getPlayer :: Game -> Int -> Maybe Player
  getPlayer game playerNo = (players game) LS.!! (playerNo - 1)

  -- |
  --    Restores a game from a list of moves. Constructs the initial game from the given players, letter bag
  --    and dictionary, then replays each move in order.
  --
  --    The letter bag must be constructed with the same tiles and random generator as the original game,
  --    the same dictionary must be used and the players must be in the original order.
  --
  --    Returns an error if the game cannot be constructed, or if any move is invalid.
  restoreGame :: (Player, Player, Maybe (Player, Maybe Player)) -> LetterBag -> Dictionary -> NE.NonEmpty Move -> Either WordifyError (NE.NonEmpty GameTransition)
  restoreGame playersSetup letterBag dict moves = do
    game <- makeGame playersSetup letterBag dict
    T.sequence $ replayMoves game moves

  -- |
  --    Like 'restoreGame' but returns a list of 'Either' so that laziness can be maintained,
  --    meaning all the game transitions don't have to be buffered before they can be consumed.
  restoreGameLazy :: (Player, Player, Maybe (Player, Maybe Player)) -> LetterBag -> Dictionary -> NE.NonEmpty Move -> Either WordifyError (NE.NonEmpty (Either WordifyError GameTransition))
  restoreGameLazy playersSetup letterBag dict moves = do
    game <- makeGame playersSetup letterBag dict
    return $ replayMoves game moves

  replayMoves :: Game -> NE.NonEmpty Move -> NE.NonEmpty (Either WordifyError GameTransition)
  replayMoves game (mv NE.:| moves) = NE.scanl nextMove (makeMove game mv) moves
    where
      nextMove transition move = transition >>= \success -> makeMove (newGame success) move
