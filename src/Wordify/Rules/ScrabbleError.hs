module Wordify.Rules.ScrabbleError
  ( ScrabbleError
      ( LetterBagFileNotOpenable,
        MalformedLetterBagFile,
        MalformedDictionaryFile,
        DictionaryFileNotFound,
        NotEnoughLettersInStartingBag,
        MisplacedLetter,
        DoesNotConnectWithWord,
        NoTilesPlaced,
        DoesNotCoverTheStarTile,
        PlacedTileOnOccupiedSquare,
        CannotPlaceBlankWithoutLetter,
        InvalidTileLetters,
        NotAssignableToBlank,
        WordsNotInDictionary,
        PlayerCannotPlace,
        GameNotInProgress,
        CannotExchangeWhenNoLettersInBag,
        PlayerCannotExchange,
        MiscError
      ),
  )
where

import Data.List
import Wordify.Rules.Pos
import Wordify.Rules.Tile

type LettersOnRack = [Tile]

type ValidBlankValues = [String]

data ScrabbleError
  = -- | The caller has supplied an invalid path to a letter bag file, or the file is not openable
    LetterBagFileNotOpenable String
  | -- | The letter bag file is marformed, so could not be parsed.
    MalformedLetterBagFile FilePath
  | -- | The path given to a dictionary file was invalid.
    DictionaryFileNotFound FilePath
  | -- | The dictionary file could not be parsed as it was malformed.
    MalformedDictionaryFile String
  | -- | A letter bag with insufficient tiles was used to create a game.
    NotEnoughLettersInStartingBag Int
  | -- | The player has made an illegal tile placement. Tiles placed must form a line of tiles.
    MisplacedLetter Pos
  | -- | The tiles the player placed do not connect with any word (applies after the first move on the board)
    DoesNotConnectWithWord
  | -- | The client put the player in the situation to be able to place no tiles.
    NoTilesPlaced
  | -- | The first move on the board does not cover the star.
    DoesNotCoverTheStarTile
  | -- | The client allowed the player to place tiles on a square that is already occupied with tiles.
    PlacedTileOnOccupiedSquare Pos Tile
  | -- | A blank tile must be labeled with a letter before being placed.
    CannotPlaceBlankWithoutLetter Pos
  | -- | The tiles the player placed formed one or more words which are not in the dictionary.
    WordsNotInDictionary [String]
  | -- | The caller allowed the client to place tiles on the board which were not in their rack.
    PlayerCannotPlace LettersOnRack [Tile]
  | -- | The string applied to the blank letter isn't a valid tile value
    NotAssignableToBlank Pos String ValidBlankValues
  | -- | The letters on the played tile aren't valid
    InvalidTileLetters Pos String
  | -- | The caller allowed the player to attempt to exchange when no letters were left in the bag.
    CannotExchangeWhenNoLettersInBag
  | -- | The caller allowed the player to attempt to exchange tiles that they do not have.
    PlayerCannotExchange LettersOnRack [Tile]
  | -- | The caller allowed a move to be made when the game is finished.
    GameNotInProgress
  | MiscError String
  deriving (Eq)

instance Show ScrabbleError where
  show (MalformedDictionaryFile reason) = "Dictionary file could not be parsed for the following reason: " ++ reason
  show (MalformedLetterBagFile path) = "Letter bag file " ++ path ++ " was malformed."
  show (DictionaryFileNotFound path) = "Dictionary file " ++ path ++ " was not found."
  show (LetterBagFileNotOpenable path) = "Letter bag file " ++ path ++ " was not openable"
  show (NotEnoughLettersInStartingBag num) = "A starting bag must have enough tiles to distribute to the players to start a game. Bag has " ++ show num ++ " tiles."
  show (MisplacedLetter pos) = "Placed tiles were not legally placed. Starting at tile placed at pos: " ++ show pos
  show (DoesNotConnectWithWord) = "Placed tiles do not connect with an existing word on the board."
  show (NoTilesPlaced) = "No tiles were placed in the move."
  show (DoesNotCoverTheStarTile) = "First move must go through the star."
  show (PlacedTileOnOccupiedSquare pos _) = "Move replaces a tile already on the board at " ++ show pos ++ ". This is not a legal move."
  show (CannotPlaceBlankWithoutLetter pos) = "A played blank tile must be given a letter. Blank tile played at " ++ show pos ++ " was not given a letter."
  show (WordsNotInDictionary xs) = "The following words are not in the scrabble dictionary: " ++ show xs
  show (InvalidTileLetters pos letters) = "A played tile is not a valid tile. " ++ letters ++ " played at " ++ show pos ++ " was not given a letter."
  show (PlayerCannotPlace letterRack tiles) = "The player cannot place: " ++ show tiles ++ ". Tiles on rack: " ++ show letterRack ++ ". Blank tiles must be labeled and the placed tiles must be on the rack."
  show (NotAssignableToBlank pos assigned validAssignments) = "Cannot assign value " ++ show assigned ++ " to blank tile. Valid values: " ++ (intercalate ", " validAssignments) ++ " Blank placed at " ++ show pos
  show (CannotExchangeWhenNoLettersInBag) = "Cannot exchange letters when there are no letters in the bag."
  show (PlayerCannotExchange letterRack tiles) = "Player does not have the letters to exchange " ++ show tiles ++ ". Tiles on rack: " ++ show letterRack ++ ". Blank tiles must not be labeled."
  show (GameNotInProgress) = "A move was attempted on a game that is not in progress."
  show (MiscError str) = str
