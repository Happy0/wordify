module Wordify.Rules.Dictionary
  ( Dictionary,
    makeDictionary,
    dictionaryFromWords,
    isValidWord,
    invalidWords,
  )
where

import Conduit (MonadIO (liftIO), MonadResource, MonadUnliftIO, runResourceT)
import qualified Control.Applicative as M
import Control.Arrow
import qualified Control.Exception as Exc
import Control.Monad
import qualified Control.Monad.Trans.Accum as Co
import qualified Data.ByteString as BS
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Combinators as Co
import qualified Data.Conduit.List as Cl
import qualified Data.Foldable as C
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import Wordify.Rules.ScrabbleError

data Dictionary = Dictionary (HashSet.HashSet T.Text) deriving (Show)

-- |
--     Returns the list of words which are not present in the given dictionary from a
--     list of input words.
invalidWords :: Dictionary -> [String] -> [String]
invalidWords dictionary = filter $ not . isValidWord dictionary

{-
   Returns true if the given word is in the given dictionary.
-}
isValidWord :: Dictionary -> String -> Bool
isValidWord (Dictionary dictionaryWords) word = HashSet.member (T.pack word) dictionaryWords

dictionaryFromWords :: [String] -> Dictionary
dictionaryFromWords words = Dictionary . HashSet.fromList $ map T.pack (upperCaseWords words)

sinkFileLinesToHashset :: (MonadResource m, MonadUnliftIO m) => FilePath -> ConduitT () Void m (HashSet.HashSet T.Text)
sinkFileLinesToHashset filePath =
  Co.sourceFile filePath .| Co.linesUnboundedAscii .| Cl.map TE.decodeUtf8 .| Co.foldl insertIntoHashet HashSet.empty
  where
    insertIntoHashet = flip HashSet.insert

loadDictionaryFromFile :: (MonadUnliftIO m, MonadResource m) => FilePath -> m Dictionary
loadDictionaryFromFile filePath = do
  hashset <- runConduit $ sinkFileLinesToHashset filePath
  pure $ Dictionary hashset
  where

-- |
--    Creates a dictionary from a file containing a list of valid words, each word being seperated by a newline.
makeDictionary :: FilePath -> IO (Either ScrabbleError Dictionary)
makeDictionary filePath = join . fmap parseDictionary <$> readDictionaryFile filePath

readDictionaryFile :: FilePath -> IO (Either ScrabbleError String)
readDictionaryFile filePath = convertFileError <$> (Exc.try (readFile filePath) :: (IO (Either Exc.IOException String)))
  where
    convertFileError = left (\_ -> DictionaryFileNotFound filePath)

parseDictionary :: String -> Either ScrabbleError Dictionary
parseDictionary = either (Left . MalformedDictionaryFile . show) (Right . dictionaryFromWords) . parseFile
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
