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

  