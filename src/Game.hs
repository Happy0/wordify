module Game(Game, makeGame, updateGame) where

  import Player
  import Board
  import Dictionary
  import LetterBag
  import Data.IntMap as IntMap
  import Data.List
  import ScrabbleError
  import Control.Applicative
  import Data.Maybe

  data Game = Game { player1 :: Player
                     , player2 :: Player
                     , optionalPlayers :: Maybe (Player, Maybe Player)
                     , board :: Board
                     , bag :: LetterBag
                     , dictionary :: Dictionary 
                     , currentPlayer :: Int
                     , moveNumber :: Int }

  makeGame :: (Player, Player, Maybe (Player, Maybe Player))-> Board -> LetterBag -> Dictionary -> Either ScrabbleError (Player, Game)
  makeGame (play1, play2, optionalPlayers) board bag dictionary = Right $ (player1, Game player1 player2 optional board finalBag dictionary 1 1)
    where
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

  updateGame :: Game -> Player -> Board -> LetterBag -> (Player, Game)
  updateGame game player newBoard newBag = (newPlayer, updatedPlayerGame {board = newBoard, bag = newBag, moveNumber = succ moveNo})
    where
      updatedPlayerGame = updateCurrentPlayer game player
      (newPlayerNum, newPlayer) = nextPlayer game
      moveNo = moveNumber game

  updateCurrentPlayer :: Game -> Player -> Game
  updateCurrentPlayer game player =
    case playing of
      1 -> game {player1 = player}
      2 -> game {player2 = player}
      3 -> game {optionalPlayers = optional >>= (\(player3, player4) -> return (player, player4)) }
      4 -> game {optionalPlayers = optional >>= (\(player3, player4) -> return (player3, (player4 >> Just player))) }

    where
      playing = currentPlayer game
      optional = optionalPlayers game

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
