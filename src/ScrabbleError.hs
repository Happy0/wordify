module ScrabbleError (ScrabbleError(LetterBagFileNotFound, MalformedLetterBagFile, MalformedDictionaryFile,
 DictionaryFileNotFound, InvalidNumberOfPlayers, NotEnoughLettersInStartingBag)  ) where

import Control.Exception
import Control.Monad.Error

data ScrabbleError = LetterBagFileNotFound String
  | MalformedLetterBagFile FilePath
  | DictionaryFileNotFound FilePath
  | MalformedDictionaryFile FilePath
  | InvalidNumberOfPlayers
  | NotEnoughLettersInStartingBag Int
  | PlayerFourOccupiedWithPlayerThreeEmpty
  | MiscError String

instance Show ScrabbleError
 where
  show (MalformedDictionaryFile path) = "Dictionary file " ++ path ++ " was malformed."
  show (MalformedLetterBagFile path) = "Letter bag file " ++ path ++ " was malformed."
  show (DictionaryFileNotFound path) = "Dictionary file " ++ path ++ " was not found."
  show (LetterBagFileNotFound path) = "Letter bag file " ++ path ++ " was not found"
  show (InvalidNumberOfPlayers) = "A game must have 2 - 4 players"
  show (NotEnoughLettersInStartingBag num) = "A starting bag must have enough tiles to distribute to the players to start a game. Bag has " ++ show num ++ " tiles."

instance Error ScrabbleError where
  noMsg = MiscError "Unexpected internal error"
  strMsg err = MiscError err
