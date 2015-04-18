module Wordify.Rules.Dictionary (Dictionary, isValidWord, makeDictionary, invalidWords, dictionaryFromWords) where

  import qualified Data.HashSet as HashSet
  import Wordify.Rules.ScrabbleError
  import qualified Control.Exception as Exc
  import Text.ParserCombinators.Parsec
  import Text.Parsec.Prim
  import Data.Char
  import Control.Monad
  import qualified Control.Applicative  as A

  data Dictionary = Dictionary (HashSet.HashSet String) deriving Show

  {- | 
     Returns the list of words which are not present in the given dictionary from a 
     list of input words.  
  -}
  invalidWords :: Dictionary -> [String] -> [String]
  invalidWords dictionary = filter $ not . isValidWord dictionary

  {-
     Returns true if the given word is in the given dictionary.
  -}
  isValidWord :: Dictionary -> String -> Bool
  isValidWord (Dictionary dictionaryWords) = flip HashSet.member dictionaryWords

  dictionaryFromWords :: [String] -> Dictionary
  dictionaryFromWords= Dictionary . HashSet.fromList . upperCaseWords 

  {- |
    Creates a dictionary from a file containing a list of valid words, each word being seperated by a newline.
  -}
  makeDictionary :: FilePath -> IO (Either ScrabbleError Dictionary)
  makeDictionary filePath = either (\_ -> Left (DictionaryFileNotFound filePath)) parseDictionary 
        <$> (Exc.try (readFile filePath) :: (IO (Either Exc.IOException String))) 

  parseDictionary :: String -> Either ScrabbleError Dictionary
  parseDictionary dictionaryString = 
    either (Left . MalformedDictionaryFile . show) (Right . dictionaryFromWords) $ parseFile dictionaryString
    where
        parseFile = parse dictionaryFile "" 

        dictionaryFile = 
            do
            dictWords <- many word
            _ <- eof
            return dictWords

        word = 
            do
            entry <- many letter :: Parser String
            _ <- newline
            return entry
 
  upperCaseWords :: [String] -> [String]
  upperCaseWords = (map . map) toUpper


