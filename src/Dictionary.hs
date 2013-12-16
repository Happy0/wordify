module Dictionary (isValidWord, makeDictionary) where

  import qualified Data.Set as Set
  import ScrabbleError
  import qualified Control.Exception as Exc
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Combinator
  import Text.Parsec.Token
  import Text.ParserCombinators.Parsec.Char
  import Data.Char

  data Dictionary = Dictionary (Set.Set String) deriving Show

  isValidWord :: Dictionary -> String -> Bool
  isValidWord (Dictionary dictionaryWords) = flip Set.member dictionaryWords

  makeDictionary :: FilePath -> IO (Either ScrabbleError Dictionary)
  makeDictionary filePath = 
    do
      fileContents <- Exc.try (readFile filePath) :: IO (Either Exc.IOException String)
      case fileContents of 
        Left e -> return $ Left (DictionaryFileNotFound filePath)
        Right dictContents -> 
          do
              let dictWords = parseFile dictContents
              case dictWords of 
                Left e -> return $ Left (MalformedDictionaryFile filePath)
                Right wordList -> return $ Right (Dictionary $ Set.fromList (toUpperCase wordList))

    where
      toUpperCase = (map . map) toUpper       
      parseFile contents = parse dictionaryFile "Malformed dictionary file " contents

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
