module Wordify.Rules.Move (
            Move(PlaceTiles, Exchange, Pass)
           ,GameTransition(MoveTransition, ExchangeTransition, PassTransition, GameFinished)
           ,makeMove
           ,newGame
           ,restoreGame
           ,restoreGameLazy) where

  import Wordify.Rules.ScrabbleError
  import Wordify.Rules.FormedWord
  import Control.Monad
  import Control.Applicative
  import Wordify.Rules.Player
  import qualified Data.Map as Map
  import Wordify.Rules.Pos
  import Wordify.Rules.Tile
  import Wordify.Rules.LetterBag
  import Wordify.Rules.Board
  import Wordify.Rules.Dictionary
  import Wordify.Rules.Game.Internal
  import Wordify.Rules.Game
  import qualified Data.List.NonEmpty as NE
  import qualified Data.Traversable as T
  import qualified Data.Map as M
  import Control.Error.Util
  import Control.Arrow

  data GameTransition = -- | The new player (with their updated letter rack and score), new game state, and the words formed by the move
                        MoveTransition Player Game FormedWords
                        -- | The new game state, and the player with their rack before and after the exchange respectively.
                        | ExchangeTransition Game Player Player
                        -- | The new game state with the opportunity to play passed on to the next player.
                        | PassTransition Game
                        {- |
                          The game has finished. The final game state, and the final words formed (if the game was ended by a
                          player placing their final tiles.) The players before their scores were increased or decreased is also
                          given.
                        -}
                        | GameFinished Game (Maybe FormedWords)

  {-|
    Transitiions the game to the next state. If the move places tiles, the player must have the tiles to place and
    place the tiles legally. If the move exchanges tiles, the bag must not be empty and the player must have the
    tiles to exchange. A ScrabbleError is returned if these condtions are not the case.
  -}
  makeMove :: Game -> Move -> Either ScrabbleError GameTransition
  makeMove game move
    | gameStatus game /= InProgress = Left GameNotInProgress
    | otherwise = flip addMoveToHistory move <$> gameTransition
    where
      gameTransition = case move of
        PlaceTiles placed -> makeBoardMove game placed
        Exchange exchanged -> exchangeMove game exchanged
        Pass -> (Right . passMove) game

  makeBoardMove :: Game -> M.Map Pos Tile -> Either ScrabbleError GameTransition
  makeBoardMove game placed =
    do
      formed <- formedWords
      (overallScore, _) <- scoresIfWordsLegal dict formed
      nextBoard <- newBoard currentBoard placed
      intermediatePlayer <- removeLettersandGiveScore player playedTiles overallScore

      if hasEmptyRack intermediatePlayer && (bagSize letterBag == 0)
       then
        do
          let beforeFinalisingGame = updateGame game intermediatePlayer nextBoard letterBag
          let finalisedGame = finaliseGame beforeFinalisingGame
          return $ GameFinished finalisedGame (Just formed)
        else
          do
            let (newPlayer, newBag) = updatePlayerRackAndBag intermediatePlayer letterBag (Map.size placed)
            let updatedGame = updateGame game newPlayer nextBoard newBag
            return $ MoveTransition newPlayer updatedGame formed
    where
      player = currentPlayer game
      playedTiles = Map.elems placed
      currentBoard = board game
      dict = dictionary game
      letterBag = bag game

      formedWords = if any isPlaceMove (movesMade game)
       then wordsFormedMidGame currentBoard placed
       else wordFormedFirstMove currentBoard placed

      isPlaceMove mv = case mv of
                          PlaceTiles _ -> True
                          _ -> False

  exchangeMove :: Game -> [Tile] -> Either ScrabbleError GameTransition
  exchangeMove game exchangedTiles =
    let exchangeOutcome = exchangeLetters (bag game) exchangedTiles
    in case exchangeOutcome of
      Nothing -> Left CannotExchangeWhenNoLettersInBag
      Just (givenTiles, newBag) ->
          let newPlayer = exchange player exchangedTiles givenTiles
          in maybe (Left $ PlayerCannotExchange (rack player) exchangedTiles) (\exchangedPlayer ->
                    let gameState = updateGame game exchangedPlayer (board game) newBag
                    in Right $ ExchangeTransition gameState player exchangedPlayer) newPlayer
    where
      player = currentPlayer game

  passMove :: Game -> GameTransition
  passMove game =
    let gameState = pass game
    in
      if gameFinished
      then GameFinished (finaliseGame gameState) Nothing
      else PassTransition gameState
    where
      numPasses = passes game + 1
      gameFinished = numPasses == numberOfPlayers game * 2

  {- |
    Restores a game from a list of moves. The game must be set up in the way the original game was set up
    (including the letter bag constructed with the same tiles and random generator, dictionary and the list of players
    in the original order.)

    If the game is not set up as it was originally, this function will return a scrabble error with the move which was invalid
    with the given state. For example, if the original players are not ordered in the correct way then the player will not have
    the required tiles to make the move.
  -}
  restoreGame :: Game -> NE.NonEmpty Move -> Either ScrabbleError (NE.NonEmpty GameTransition)
  restoreGame game = T.sequence . restoreGameLazy game

  {- |
    Maps each move to a resulting game transition, if the move is legal. Has the same semantics as 'restoreGame'
    but returns a list of 'Either' so that laziness can be maintained, meaning all the game transitions
    dont have to be buffered before they can be consumed.
  -}
  restoreGameLazy :: Game -> NE.NonEmpty Move -> NE.NonEmpty (Either ScrabbleError GameTransition)
  restoreGameLazy game (mv NE.:| moves) = NE.scanl nextMove (makeMove game mv) moves
    where
      nextMove transition move = transition >>= \success -> makeMove (newGame success) move

  newGame :: GameTransition -> Game
  newGame (MoveTransition _ game _) = game
  newGame (ExchangeTransition game _ _) = game
  newGame (PassTransition game) = game
  newGame (GameFinished game _) = game

  addMoveToHistory :: GameTransition -> Move -> GameTransition
  addMoveToHistory (MoveTransition player game formedWords) move = MoveTransition player (updateHistory game move) formedWords
  addMoveToHistory (ExchangeTransition game oldPlayer newPlayer ) move = ExchangeTransition (updateHistory game move) oldPlayer newPlayer
  addMoveToHistory (PassTransition game) move = PassTransition (updateHistory game move)
  addMoveToHistory (GameFinished game wordsFormed) move = GameFinished (updateHistory game move) wordsFormed

  finaliseGame :: Game -> Game
  finaliseGame game
    | gameStatus game == Finished = game
    | otherwise = game {player1 = play1, player2 = play2, optionalPlayers = optionals, gameStatus = Finished, moveNumber = pred moveNo}
      where
        unplayedValues = Prelude.sum $ Prelude.map tileValues allPlayers
        allPlayers = players game
        moveNo = moveNumber game

        play1 = finalisePlayer (player1 game)
        play2 = finalisePlayer (player2 game)
        optionals = optionalPlayers game >>= (\(player3, maybePlayer4) ->
            Just (finalisePlayer player3, finalisePlayer <$> maybePlayer4 ) )

        finalisePlayer player = if hasEmptyRack player then giveEndWinBonus player unplayedValues
          else giveEndLosePenalty player (tileValues player)

  updatePlayerRackAndBag :: Player -> LetterBag -> Int -> (Player, LetterBag)
  updatePlayerRackAndBag player letterBag numPlayed
    | tilesInBag == 0 = (player, letterBag)
    | tilesInBag >= numPlayed =
       maybe (player, letterBag) (first (giveTiles player)) $ takeLetters letterBag numPlayed
    | otherwise = maybe (player, letterBag) (first (giveTiles player)) $ takeLetters letterBag tilesInBag
    where
      tilesInBag = bagSize letterBag

  newBoard :: Board -> M.Map Pos Tile -> Either ScrabbleError Board
  newBoard currentBoard placed = foldM (\oldBoard (pos, tile) -> newBoardIfUnoccupied oldBoard pos tile) currentBoard $ Map.toList placed
    where
      newBoardIfUnoccupied brd pos tile = note (PlacedTileOnOccupiedSquare pos tile) $ placeTile brd tile pos


  removeLettersandGiveScore :: Player -> [Tile] -> Int -> Either ScrabbleError Player
  removeLettersandGiveScore player playedTiles justScored =
    let newPlayer = flip increaseScore justScored <$> removePlayedTiles player playedTiles
    in note (PlayerCannotPlace (rack player) playedTiles) newPlayer

  scoresIfWordsLegal :: Dictionary -> FormedWords -> Either ScrabbleError (Int, [(String, Int)])
  scoresIfWordsLegal dict formedWords =
    let strings = wordStrings formedWords
    in case invalidWords dict strings of
      []-> Right $ wordsWithScores formedWords
      xs -> Left $ WordsNotInDictionary xs
