module Wordify.Rules.Move
  ( Move (PlaceTiles, Exchange, Pass),
    GameTransition (MoveTransition, ExchangeTransition, PassTransition, GameFinished),
    makeMove,
    newGame,
  )
where

import Control.Applicative
import Control.Arrow
import Control.Error
import Control.Error.Util
import Control.Monad
import Data.Map
import qualified Data.Map as M
import qualified Data.Map as Map
import Wordify.Rules.Board
import Wordify.Rules.Dictionary
import Wordify.Rules.FormedWord
import Wordify.Rules.Game.Internal
import Wordify.Rules.LetterBag
import Wordify.Rules.Player
import Wordify.Rules.Pos
import Wordify.Rules.WordifyError
import Wordify.Rules.Tile

data GameTransition
  = -- | The new player (with their updated letter rack and score), new game state, and the words formed by the move
    MoveTransition Player Game FormedWords
  | -- | The new game state, the player with their rack before and after the exchange respectively.and the exchanged tiles
    ExchangeTransition Game Player Player [Tile]
  | -- | The new game state with the opportunity to play passed on to the next player.
    PassTransition Game
  | -- |
    --                          The game has finished. The final game state, and the final words formed (if the game was ended by a
    --                          player placing their final tiles.) The players before their scores were increased or decreased is also
    --                          given.
    GameFinished Game (Maybe FormedWords)

-- |
--    Transitiions the game to the next state. If the move places tiles, the player must have the tiles to place and
--    place the tiles legally. If the move exchanges tiles, the bag must not be empty and the player must have the
--    tiles to exchange. A WordifyError is returned if these condtions are not the case.
makeMove :: Game -> Move -> Either WordifyError GameTransition
makeMove game move
  | gameStatus game /= InProgress = Left GameNotInProgress
  | otherwise = flip addMoveToHistory move <$> gameTransition
  where
    gameTransition = case move of
      PlaceTiles placed -> makeBoardMove game placed
      Exchange exchanged -> exchangeMove game exchanged
      Pass -> (Right . passMove) game

makeBoardMove :: Game -> M.Map Pos Tile -> Either WordifyError GameTransition
makeBoardMove game placed =
  do
    validPlacedTiles <- validateTiles (validLetters letterBag) placed
    let playedTiles = Map.elems validPlacedTiles
    formed <- formedWords
    (overallScore, _) <- scoresIfWordsLegal dict formed
    nextBoard <- newBoard currentBoard validPlacedTiles
    intermediatePlayer <- removeLettersandGiveScore player playedTiles overallScore

    if hasEmptyRack intermediatePlayer && (bagSize letterBag == 0)
      then do
        let beforeFinalisingGame = updateGame game intermediatePlayer nextBoard letterBag
        let finalisedGame = finaliseGame beforeFinalisingGame
        return $ GameFinished finalisedGame (Just formed)
      else do
        let (newPlayer, newBag) = updatePlayerRackAndBag intermediatePlayer letterBag (Map.size validPlacedTiles)
        let updatedGame = updateGame game newPlayer nextBoard newBag
        return $ MoveTransition newPlayer updatedGame formed
  where
    player = currentPlayer game
    currentBoard = board game
    dict = dictionary game
    letterBag = bag game
    validTiles = validLetters letterBag

    formedWords =
      if any isPlaceMove (movesMade game)
        then wordsFormedMidGame currentBoard placed
        else wordFormedFirstMove currentBoard placed

    isPlaceMove mv = case mv of
      PlaceTiles _ -> True
      _ -> False

exchangeMove :: Game -> [Tile] -> Either WordifyError GameTransition
exchangeMove game exchangedTiles =
  let exchangeOutcome = exchangeLetters (bag game) exchangedTiles
   in case exchangeOutcome of
        Nothing -> Left CannotExchangeWhenNoLettersInBag
        Just (givenTiles, newBag) ->
          let newPlayer = exchange player exchangedTiles givenTiles
           in maybe
                (Left $ PlayerCannotExchange (tilesOnRack player) exchangedTiles)
                ( \exchangedPlayer ->
                    let gameState = updateGame game exchangedPlayer (board game) newBag
                     in Right $ ExchangeTransition gameState player exchangedPlayer exchangedTiles
                )
                newPlayer
  where
    player = currentPlayer game

passMove :: Game -> GameTransition
passMove game =
  let gameState = pass game
   in if gameFinished
        then GameFinished (finaliseGame gameState) Nothing
        else PassTransition gameState
  where
    numPasses = passes game + 1
    gameFinished = numPasses == numberOfPlayers game * 2


validateTiles :: ValidTiles -> M.Map Pos Tile -> Either WordifyError (M.Map Pos Tile)
validateTiles validTiles placed = fromList <$> mapM (validateTilePlacement validTiles) (toList placed)
  where
    validateTilePlacement :: ValidTiles -> (Pos, Tile) -> Either WordifyError (Pos, Tile)
    validateTilePlacement validTiles (pos, Letter letters x) = (,) pos <$> note (InvalidTileLetters pos letters) (Map.lookup letters validTiles)
    validateTilePlacement validTiles (pos, Blank (Just assigned)) =
      note (NotAssignableToBlank pos assigned validTileStrings) (Map.lookup assigned validTiles) >>= \x -> Right (pos, Blank (Just assigned))
    validateTilePlacement validTiles (pos, Blank Nothing) = Left (CannotPlaceBlankWithoutLetter pos)

    validTileStrings = Map.keys validTiles

newGame :: GameTransition -> Game
newGame (MoveTransition _ game _) = game
newGame (ExchangeTransition game _ _ _) = game
newGame (PassTransition game) = game
newGame (GameFinished game _) = game

addMoveToHistory :: GameTransition -> Move -> GameTransition
addMoveToHistory (MoveTransition player game formedWords) move = MoveTransition player (updateHistory game move) formedWords
addMoveToHistory (ExchangeTransition game oldPlayer newPlayer exchangedTiles) move = ExchangeTransition (updateHistory game move) oldPlayer newPlayer exchangedTiles
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
    optionals =
      optionalPlayers game
        >>= ( \(player3, maybePlayer4) ->
                Just (finalisePlayer player3, finalisePlayer <$> maybePlayer4)
            )

    finalisePlayer player =
      if hasEmptyRack player
        then giveEndWinBonus player unplayedValues
        else giveEndLosePenalty player (tileValues player)

updatePlayerRackAndBag :: Player -> LetterBag -> Int -> (Player, LetterBag)
updatePlayerRackAndBag player letterBag numPlayed
  | tilesInBag == 0 = (player, letterBag)
  | tilesInBag >= numPlayed =
    maybe (player, letterBag) (first (giveTiles player)) $ takeLetters letterBag numPlayed
  | otherwise = maybe (player, letterBag) (first (giveTiles player)) $ takeLetters letterBag tilesInBag
  where
    tilesInBag = bagSize letterBag

newBoard :: Board -> M.Map Pos Tile -> Either WordifyError Board
newBoard currentBoard placed = foldM (\oldBoard (pos, tile) -> newBoardIfUnoccupied oldBoard pos tile) currentBoard $ Map.toList placed
  where
    newBoardIfUnoccupied brd pos tile = note (PlacedTileOnOccupiedSquare pos tile) $ placeTile brd tile pos

removeLettersandGiveScore :: Player -> [Tile] -> Int -> Either WordifyError Player
removeLettersandGiveScore player playedTiles justScored = do
  let newPlayer = flip increaseScore justScored <$> removePlayedTiles player playedTiles
   in note (PlayerCannotPlace (tilesOnRack player) playedTiles) newPlayer

scoresIfWordsLegal :: Dictionary -> FormedWords -> Either WordifyError (Int, [(String, Int)])
scoresIfWordsLegal dict formedWords =
  let strings = wordStrings formedWords
   in case invalidWords dict strings of
        [] -> Right $ wordsWithScores formedWords
        xs -> Left $ WordsNotInDictionary xs
