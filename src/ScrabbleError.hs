module ScrabbleError (ScrabbleError(LetterBagFileNotFound, MalformedLetterBagFile,
 MalformedDictionaryFile, DictionaryFileNotFound, NotEnoughLettersInStartingBag,
  MisplacedLetter, DoesNotConnectWithWord, NoTilesPlaced, DoesNotIntersectCoverTheStarTile,
   PlacedTileOnOccupiedSquare, CannotPlaceBlankWithoutLetter, WordsNotInDictionary, PlayerCannotPlace,
   GameNotInProgress, CannotExchangeWhenNoLettersInBag)) where

  import Control.Exception
  import Control.Monad.Error
  import Pos
  import Square
  import Tile
  import Player

  data ScrabbleError = LetterBagFileNotFound String
    | MalformedLetterBagFile FilePath
    | DictionaryFileNotFound FilePath
    | MalformedDictionaryFile FilePath
    | NotEnoughLettersInStartingBag Int
    | MisplacedLetter Pos Square
    | DoesNotConnectWithWord
    | NoTilesPlaced
    | DoesNotIntersectCoverTheStarTile
    | PlacedTileOnOccupiedSquare
    | CannotPlaceBlankWithoutLetter
    | WordsNotInDictionary [String]
    | PlayerCannotPlace LetterRack [Tile]
    | GameNotInProgress
    | CannotExchangeWhenNoLettersInBag
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
    show (PlacedTileOnOccupiedSquare) = "Move replaces a tile already on the board. This is not a legal move."
    show (CannotPlaceBlankWithoutLetter) = "A played blank letter at must be given a letter."
    show (WordsNotInDictionary xs) = "The following words are not in the scrabble dictionary: " ++ show xs
    show (PlayerCannotPlace rack tiles) = "The player cannot place: " ++ show tiles ++ ". Tiles on rack: " ++ show rack ++ ". Blank tiles must be labeled and the placed tiles must be on the rack."
    show (CannotExchangeWhenNoLettersInBag) = "Cannot exchange letters when there are no letters in the bag."
    show (GameNotInProgress) = "A move was attempted on a game that is not in progress."

  instance Error ScrabbleError where
    noMsg = MiscError "Unexpected internal error"
    strMsg err = MiscError err
