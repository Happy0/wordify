module ScrabbleError (ScrabbleError(LetterBagFileNotFound, MalformedLetterBagFile, MalformedDictionaryFile, DictionaryFileNotFound)  ) where

import Control.Exception
import Control.Monad.Error

data ScrabbleError = LetterBagFileNotFound String
	| MalformedLetterBagFile FilePath
  | DictionaryFileNotFound FilePath
  | MalformedDictionaryFile FilePath
	| MiscError String

instance Show ScrabbleError
 where
  show (MalformedDictionaryFile path) = "Dictionary file " ++ path ++ " was malformed.";
	show (MalformedLetterBagFile path) = "Letter bag file " ++ path ++ " was malformed.";
  show (DictionaryFileNotFound path) = "Dictionary file " ++ path ++ " was not found.";
	show (LetterBagFileNotFound path) = "Letter bag file " ++ path ++ " was not found";


instance Error ScrabbleError where
	noMsg = MiscError "Unknown error"
	strMsg err = MiscError err
