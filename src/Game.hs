module Game(Game, player1, player2, optionalPlayers,
 board, bag, dictionary, currentPlayer, moveNumber, makeGame, updateGame) where

  import Player
  import Board
  import Dictionary
  import LetterBag
  import Data.IntMap as IntMap
  import Data.List
  import ScrabbleError
  import Data.Maybe
  import Control.Applicative

  data Game = Game { player1 :: Player
                     , player2 :: Player
                     , optionalPlayers :: Maybe (Player, Maybe Player)
                     , board :: Board
                     , bag :: LetterBag
                     , dictionary :: Dictionary 
                     , currentPlayer :: Int
                     , moveNumber :: Int } deriving Show
  
  {-
    Starts a new game. 

    A game has at least 2 players, and 2 optional players (player 3 and 4.) The players should be newly created,
    as tiles from the letter bag will be distributed to each player.

    Takes a letter bag and dictionary, which should be localised to the same language.

    Yields a tuple with the first player and the initial game state. Returns a 'Left' if there are not enough
    tiles in the letter bag to distribute to the players.
  -}
  makeGame :: (Player, Player, Maybe (Player, Maybe Player)) -> LetterBag -> Dictionary -> Either ScrabbleError (Player, Game)
  makeGame (play1, play2, optionalPlayers) bag dictionary =
   if (numberOfPlayers * 7 > lettersInBag) then Left (NotEnoughLettersInStartingBag lettersInBag)
    else Right $ (player1, Game player1 player2 optional emptyBoard finalBag dictionary 1 1)
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

  {-
    Updates the game with the new board and letter bag state, and the last player to play's state after replacing their played
    tiles with new tiles from the letter bag. 

    Yields a tuple with the next player to play, and the current game state.
  -}
  updateGame :: Game -> Player -> Board -> LetterBag -> (Player, Game)
  updateGame game player newBoard newBag = (newPlayer, updatedPlayerGame {board = newBoard, bag = newBag, currentPlayer = newPlayerNum, moveNumber = succ moveNo})
    where
      updatedPlayerGame = updateCurrentPlayer game player
      (newPlayerNum, newPlayer) = nextPlayer game
      moveNo = moveNumber game

  updateCurrentPlayer :: Game -> Player -> Game
  updateCurrentPlayer game player =
    case playing of
      1 -> game {player1 = player}
      2 -> game {player2 = player}
      3 -> game {optionalPlayers = (\(player3, player4) -> (player, player4)) <$> optional }
      4 -> game {optionalPlayers = (\(player3, player4) -> (player3, (player4 >> Just player))) <$> optional  }

    where
      playing = currentPlayer game
      optional = optionalPlayers game

  {- Returns the next player to play. If there are optional players, loops back round to 'player 1' where appropriate. -}
  nextPlayer :: Game -> (Int, Player)
  nextPlayer game 
    | (playing == 1) = (2, playr2)
    | (playing == 2 || playing == 3) =
     maybe ( (1, playr1) ) (\(player3, player4) -> 
      if (playing == 2) then (3, player3 )
      else 
        case player4 of
          Just (player4) -> (4, player4)
          Nothing -> (1, playr1)
        ) $ optional
    | (playing == 4) = (1, playr1)

    where
      playing = currentPlayer game
      playr2 = player2 game
      playr1 = player1 game
      optional = optionalPlayers game

