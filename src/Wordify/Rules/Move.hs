module Wordify.Rules.Move (makeMove 
            ,Move(PlaceTiles, Exchange, Pass)
            ,GameTransition(MoveTransition, ExchangeTransition, PassTransition, GameFinished)
            ,restoreGame
            ,restoreGameLazy
            ,newGame) where

  import Wordify.Rules.ScrabbleError
  import Wordify.Rules.FormedWord
  import Control.Monad
  import Control.Applicative
  import Data.Maybe
  import Wordify.Rules.Game
  import Wordify.Rules.Player
  import qualified Data.Map as Map
  import Wordify.Rules.Pos
  import Wordify.Rules.Tile
  import Wordify.Rules.LetterBag
  import Wordify.Rules.Board
  import Wordify.Rules.Dictionary
  import qualified Data.Foldable as F
  import Wordify.Rules.Game.Internal
  import Wordify.Rules.Game
  import Data.Maybe
  import qualified Data.List.NonEmpty as NE
  import qualified Data.Traversable as T

  import qualified Data.Map as M
  import Data.Char

  data GameTransition = MoveTransition Game FormedWords
                        | ExchangeTransition Game Player Player 
                        | PassTransition Game 
                        | GameFinished Game (Maybe FormedWords) [Player]

  makeMove :: Game -> Move -> Either ScrabbleError GameTransition
  makeMove game move
    | (not $ gameStatus game == InProgress) = Left GameNotInProgress
    | otherwise = flip addMoveToHistory move <$> gameTransition
    where
      gameTransition = case move of
        PlaceTiles placed -> makeBoardMove game placed
        Exchange exchanged -> exchangeMove game exchanged
        Pass -> passMove game

  makeBoardMove :: Game -> M.Map Pos Tile -> Either ScrabbleError GameTransition
  makeBoardMove game placed =
    do
      formed <- formedWords
      (overallScore, _) <- scoresIfWordsLegal dict formed
      board <- newBoard currentBoard placed 
      player <- removeLettersandGiveScore player playedTiles overallScore

      if hasEmptyRack player && (bagSize letterBag == 0)
       then
        do
          let beforeFinalisingGame = updateGame game player board letterBag
          let finalisedGame = finaliseGame beforeFinalisingGame
          return $ GameFinished finalisedGame (Just formed) (players beforeFinalisingGame)
        else
          do
            let (newPlayer, newBag) = updatePlayerRackAndBag player letterBag (Map.size placed)
            let updatedGame = updateGame game newPlayer board newBag
            return $ MoveTransition updatedGame formed
    where
      player = currentPlayer game
      playedTiles = Map.elems placed
      currentBoard = board game
      moveNo = moveNumber game
      dict = dictionary game
      letterBag = bag game

      formedWords = if (any isPlaceMove (movesMade game))
       then wordsFormedMidGame currentBoard placed
       else wordFormedFirstMove currentBoard placed 

      isPlaceMove mv = case mv of 
                          PlaceTiles _ -> True
                          otherwise -> False
      

  exchangeMove :: Game -> [Tile] -> Either ScrabbleError GameTransition
  exchangeMove game tiles =
    let exchangeOutcome = exchangeLetters (bag game) tiles
    in case exchangeOutcome of
      Nothing -> Left CannotExchangeWhenNoLettersInBag
      Just (givenTiles, newBag) -> 
          let newPlayer = exchange player tiles givenTiles
          in maybe (Left $ PlayerCannotExchange (rack player) tiles) (\exchangedPlayer ->
                    let newGame = updateGame game exchangedPlayer (board game) newBag
                    in Right $ ExchangeTransition newGame player exchangedPlayer) newPlayer
    where
      player = currentPlayer game

  passMove :: Game -> Either ScrabbleError GameTransition
  passMove game = 
    let newGame = pass game 
    in 
      if gameFinished
      then Right $ GameFinished (finaliseGame newGame) Nothing (players newGame)
      else Right $ PassTransition newGame
    where
      numPasses = passes game + 1
      gameFinished = numPasses == ((numberOfPlayers game) * 2)

  {- 
    Restores a game from a list of moves. The game must be set up in the way the original game was set up
    (including the letter bag constructed with the same tiles and random generator, dictionary and the list of players
    in the original order.)
 
    If the game is not set up as it was originally, this function will return a scrabble error with the move which was invalid
    with the given state. For example, if the original players are not ordered in the correct way then the player will not have
    the required tiles to make the move.
  -}
  restoreGame :: Game -> NE.NonEmpty Move -> Either ScrabbleError (NE.NonEmpty GameTransition)
  restoreGame game = T.sequence . restoreGameLazy game
  
  {-
    Maps each move to a resulting game transition, if the move is legal. Has the same semantics as 'restoreGame'
    but returns a list of 'Either' so that laziness can be maintained, meaning all the game transitions
    dont have to be buffered before they can be consumed.  
  -}
  restoreGameLazy :: Game -> NE.NonEmpty Move -> NE.NonEmpty (Either ScrabbleError GameTransition)
  restoreGameLazy game (mv NE.:| moves) = NE.scanl nextMove (makeMove game mv) moves 
    where
      nextMove transition mv = transition >>= \success -> makeMove (newGame success) mv

  newGame :: GameTransition -> Game
  newGame (MoveTransition game _) = game
  newGame (ExchangeTransition game _ _) = game
  newGame (PassTransition game) = game
  newGame (GameFinished game _ _) = game

  addMoveToHistory :: GameTransition -> Move -> GameTransition
  addMoveToHistory (MoveTransition game formedWords) move = MoveTransition (updateHistory game move) formedWords
  addMoveToHistory (ExchangeTransition game oldPlayer newPlayer ) move = ExchangeTransition (updateHistory game move) oldPlayer newPlayer
  addMoveToHistory (PassTransition game) move = PassTransition (updateHistory game move)
  addMoveToHistory (GameFinished game wordsFormed players) move = GameFinished (updateHistory game move) wordsFormed players

  finaliseGame :: Game -> Game
  finaliseGame game
    | (gameStatus game == Finished) = game
    | otherwise = game {player1 = play1, player2 = play2, optionalPlayers = optional, gameStatus = Finished, moveNumber = pred moveNo}
      where
        unplayedValues = Prelude.sum $ Prelude.map tileValues allPlayers
        allPlayers = players game
        moveNo = moveNumber game

        play1 = finalisePlayer (player1 game)
        play2 = finalisePlayer (player2 game)
        optional = optionalPlayers game >>= (\(player3, maybePlayer4) ->
            Just (finalisePlayer player3, (\play4 -> finalisePlayer play4) <$> maybePlayer4 ) )

        finalisePlayer player = if hasEmptyRack player then increaseScore player unplayedValues
          else reduceScore player (tileValues player) 

  updatePlayerRackAndBag :: Player -> LetterBag -> Int -> (Player, LetterBag)
  updatePlayerRackAndBag player letterBag numPlayed =
    if tilesInBag == 0 
    then (player, letterBag)
    else
        if (tilesInBag >= numPlayed)
        then maybe (player, letterBag) (\(taken, newBag) -> 
          (giveTiles player taken, newBag)) $ takeLetters letterBag numPlayed
        else maybe (player, letterBag) (\(taken, newBag) -> 
          (giveTiles player taken, newBag)) $ takeLetters letterBag tilesInBag
    where
      tilesInBag = bagSize letterBag

  newBoard :: Board -> M.Map Pos Tile -> Either ScrabbleError Board
  newBoard board placed = foldM (\board (pos, tile) -> newBoardIfUnoccupied board pos tile) board $ Map.toList placed
    where
      newBoardIfUnoccupied board pos tile = maybe (Left $ PlacedTileOnOccupiedSquare pos tile) Right $ placeTile board tile pos

  
  removeLettersandGiveScore :: Player -> [Tile] -> Int -> Either ScrabbleError Player
  removeLettersandGiveScore player tiles justScored = 
    let newPlayer = removePlayedTiles player tiles 
    in case newPlayer of
      Nothing -> Left $ PlayerCannotPlace (rack player) tiles
      Just (playerUpdatedRack) -> Right $ increaseScore playerUpdatedRack justScored
    

  scoresIfWordsLegal :: Dictionary -> FormedWords -> Either ScrabbleError (Int, [(String, Int)])
  scoresIfWordsLegal dict formedWords = 
    let strings = wordStrings formedWords
    in case invalidWords dict strings of
      (x:xs) -> Left $ WordsNotInDictionary (x:xs)
      otherwise -> Right $ wordsWithScores formedWords




