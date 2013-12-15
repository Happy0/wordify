module ScrabbleError (ScrabbleError(LetterBagFileNotFound, MalformedLetterBagFile)  ) where

import Control.Exception
import Control.Monad.Error

data ScrabbleError = LetterBagFileNotFound String
	| MalformedLetterBagFile FilePath
	| MiscError String

instance Show ScrabbleError where
	show (MalformedLetterBagFile path) = 
		"Letter bag file " ++ path ++ " was malformed."
	show (LetterBagFileNotFound path) = "No letter bag file found at " ++ path


instance Error ScrabbleError where
	noMsg = MiscError "Unknown error"
	strMsg err = MiscError err
