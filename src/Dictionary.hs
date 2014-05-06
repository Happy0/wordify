module Dictionary (Dictionary, isValidWord, makeDictionary, invalidWords) where

  import qualified Data.HashSet as HashSet
  import ScrabbleError
  import qualified Control.Exception as Exc
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Combinator
  import Text.Parsec.Token
  import Text.ParserCombinators.Parsec.Char
  import Data.Char
  import Control.Monad

  data Dictionary = Dictionary (HashSet.HashSet String) deriving Show

  invalidWords :: Dictionary -> [String] -> [String]
  invalidWords dictionary = filter $ not . isValidWord dictionary

  isValidWord :: Dictionary -> String -> Bool
  isValidWord (Dictionary dictionaryWords) = flip HashSet.member dictionaryWords

  makeDictionary :: FilePath -> IO (Either ScrabbleError Dictionary)
  makeDictionary filePath = 
    do
      fileContents <- Exc.try (readFile filePath) :: IO (Either Exc.IOException String)
      case fileContents of 
        Left _ -> return $ Left (DictionaryFileNotFound filePath)
        Right dictContents -> 
          do
              let dictWords = parseFile dictContents
              case dictWords of 
                Left _ -> return $ Left (MalformedDictionaryFile filePath)
                Right wordList -> return $ Right (Dictionary $ HashSet.fromList wordList)

    where
      toUpperCase = (map . map) toUpper       
      parseFile contents = liftM toUpperCase $ parse dictionaryFile "Malformed dictionary file " contents

      dictionaryFile = 
        do
          dictWords <- many word
          eof
          return dictWords

      word = 
        do
          entry <- many letter
          newline
          return entry

