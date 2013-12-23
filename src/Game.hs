module Game(makeGame) where

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
                     , player3 :: Maybe Player
                     , player4 :: Maybe Player
                     , board :: Board
                     , bag :: LetterBag
                     , dictionary :: Dictionary 
                     , currentPlayer :: Int
                     , moveNumber :: Int }

  makeGame :: (Player, Player, Maybe Player, Maybe Player)-> Board -> LetterBag -> Dictionary -> Either ScrabbleError (Player, Game)
  makeGame (play1, play2, play3, play4) board bag dictionary = 
    if numTiles < numPlayers * 7 then Left(NotEnoughLettersInStartingBag numTiles) else
    Right (player1, Game player1 player2 player3 player4 board initialisedBag dictionary 1 1)

    where
      numTiles = bagSize bag
      numPlayers = length $ catMaybes [Just play1, Just play2, play3, play4]
      (player1, firstBag) = givePlayerTiles play1 bag
      (player2, secondBag) = givePlayerTiles play2 secondBag
      (player3, thirdBag) = maybe (play3, secondBag) (\player -> justPlayer $ givePlayerTiles player secondBag) play3
      (player4, initialisedBag) = maybe (play4, thirdBag) (\player -> justPlayer $ givePlayerTiles player thirdBag) play4
      givePlayerTiles :: Player -> LetterBag -> (Player, LetterBag)
      givePlayerTiles player bag = maybe (player, bag) (\(tiles, newBag) -> (giveTiles player tiles, newBag) ) $ takeLetters bag 7
      justPlayer (player, bag) = (Just player, bag)

  -- updateGame :: Game -> 

  nextPlayer :: Game -> (Player, Game)
  nextPlayer game = (player, game {currentPlayer = playerNo, moveNumber = succ $ moveNumber game})
    
    where
      (playerNo, player) = head $ drop playing sequenceOfTurns 
      playing = currentPlayer game
      -- plz. im sorry
      sequenceOfTurns = concat $ repeat $ zip [1 .. ] $ catMaybes [Just (player1 game), Just (player2 game), (player3 game), (player4 game)]

  updateCurrentPlayer :: Game -> Player -> Game
  updateCurrentPlayer game player
    | (playing == 1) = game { player1 = player }
    | (playing == 2) = game { player2  = player }
    | (playing == 3) = game { player3 = Just $ player }
    | (playing == 4) = game { player4 = Just $ player }
    where
      playing = currentPlayer game
