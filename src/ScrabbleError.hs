module ScrabbleError (ScrabbleError(LetterBagFileNotFound, MalformedLetterBagFile,
 MalformedDictionaryFile, DictionaryFileNotFound, NotEnoughLettersInStartingBag,
  MisplacedLetter, DoesNotConnectWithWord, NoTilesPlaced, DoesNotIntersectCoverTheStarTile)) where

  import Control.Exception
  import Control.Monad.Error
  import Pos
  import Square

  data ScrabbleError = LetterBagFileNotFound String
    | MalformedLetterBagFile FilePath
    | DictionaryFileNotFound FilePath
    | MalformedDictionaryFile FilePath
    | NotEnoughLettersInStartingBag Int
    | MisplacedLetter Pos Square
    | DoesNotConnectWithWord
    | NoTilesPlaced
    | DoesNotIntersectCoverTheStarTile
    | MiscError String

  instance Show ScrabbleError
   where
    show (MalformedDictionaryFile path) = "Dictionary file " ++ path ++ " was malformed."
    show (MalformedLetterBagFile path) = "Letter bag file " ++ path ++ " was malformed."
    show (DictionaryFileNotFound path) = "Dictionary file " ++ path ++ " was not found."
    show (LetterBagFileNotFound path) = "Letter bag file " ++ path ++ " was not found"
    show (NotEnoughLettersInStartingBag num) = "A starting bag must have enough tiles to distribute to the players to start a game. Bag has " ++ show num ++ " tiles."
    show (MisplacedLetter pos square) = "Placed tiles were not legally placed. Starting at tile placed at pos: " ++ show pos
    show (DoesNotConnectWithWord) = "Placed tiles do not connect with an existing word on the board."
    show (NoTilesPlaced) = "No tiles were placed in the move."
    show (DoesNotIntersectCoverTheStarTile) = "First move must go through the star."

  instance Error ScrabbleError where
    noMsg = MiscError "Unexpected internal error"
    strMsg err = MiscError err
