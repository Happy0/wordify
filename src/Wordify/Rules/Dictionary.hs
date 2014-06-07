module Wordify.Rules.Dictionary (Dictionary, isValidWord, makeDictionary, invalidWords) where

  import qualified Data.HashSet as HashSet
  import Wordify.Rules.ScrabbleError
  import qualified Control.Exception as Exc
  import Text.ParserCombinators.Parsec
  import Data.Char
  import Control.Monad

  data Dictionary = Dictionary (HashSet.HashSet String) deriving Show

  {- | 
     Returns the list of words which are not present in the given dictionary from a 
     list of input words.  
  -}
  invalidWords :: Dictionary -> [String] -> [String]
  invalidWords dictionary = filter $ not . isValidWord dictionary

  {-
     RReturns true if the given word is in the given dictionary.
  -}
  isValidWord :: Dictionary -> String -> Bool
  isValidWord (Dictionary dictionaryWords) = flip HashSet.member dictionaryWords

  {- |
    Creates a dictionary from a file containing a list of valid words, each word being seperated by a newline.
  -}
  makeDictionary :: FilePath -> IO (Either ScrabbleError Dictionary)
  makeDictionary filePath = 
    do
      fileContents <- Exc.try (readFile filePath) :: IO (Either Exc.IOException String)
      case fileContents of 
        Left _ -> return $ Left (DictionaryFileNotFound filePath)
        Right dictContents -> 
              let dictWords = parseFile dictContents
              in case dictWords of 
                Left _ -> return $ Left (MalformedDictionaryFile filePath)
                Right wordList -> return $ Right (Dictionary $ HashSet.fromList wordList)

    where
      toUpperCase = (map . map) toUpper       
      parseFile contents = liftM toUpperCase $ parse dictionaryFile "Malformed dictionary file " contents

      dictionaryFile = 
        do
          dictWords <- many word
          _ <- eof
          return dictWords

      word = 
        do
          entry <- many letter
          _ <- newline
          return entry

