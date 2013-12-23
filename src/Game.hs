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

          fillOptional (thirdPlayer, Nothing) bag = (Just (player3, Nothing), thirdBag)
            where
              (player3, thirdBag) = givePlayerTiles thirdPlayer bag
          
          fillOptional (thirdPlayer, (Just fourthPlayer)) bag = (Just (player3, Just (player4)), fourthBag)
            where
              (player3, thirdBag) = givePlayerTiles thirdPlayer bag
              (player4, fourthBag) = givePlayerTiles fourthPlayer thirdBag

          givePlayerTiles player bag = maybe (player, bag) (\(tiles, newBag) -> (giveTiles player tiles, newBag) ) $ takeLetters bag 7