module LetterBag (LetterBag, makeBag, takeLetters, exchangeLetters, shuffleBag, bagSize) where

import Tile
import System.Random
import Data.Array.IO
import Control.Monad
import qualified Control.Exception as Exc
import ScrabbleError
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.Token
import Text.ParserCombinators.Parsec.Char
import Data.Char

data LetterBag = LetterBag { tiles :: [Tile],  bagSize :: Int } deriving Show

makeBag :: FilePath -> IO (Either ScrabbleError (LetterBag))
makeBag path =
  do
    fileContents <- Exc.try (readFile path) :: IO (Either Exc.IOException String)
    case fileContents of 
      Left e ->
        return $ Left (LetterBagFileNotFound path)
      Right str -> do
          let tiles = parseBag str

          case tiles of 
            Left _ -> 
               return $ Left (MalformedLetterBagFile path)
            Right list -> do
               let letterBag = LetterBag list (length list)
               shuffledBag <- shuffleBag letterBag
               return (Right shuffledBag)

{-
  Takes 'n' numbers from a letter bag, yielding 'Nothing'
  if there is not enough tiles left in the bag or a 'Just'
  tuple where the left value is the taken tiles, and the right
  value is the new bag.
-}
takeLetters :: LetterBag -> Int -> Maybe ([Tile], LetterBag)
takeLetters (LetterBag tiles lettersLeft) numTake =
  if (newNumLetters < 0) then Nothing
   else Just (taken, LetterBag newLetters newNumLetters)
  where
    newNumLetters = lettersLeft - numTake
    (taken, newLetters) = splitAt numTake tiles

{-
  Exchanges given tiles for the same number of tiles from the bag.
  The exchanged letters are added to the bag, the bag is then shuffled, 
  and then the same number of tiles as exchanged are drawn from the bag.

  Returns 'Nothing' if there are not enough letters in the bag to exchange
  the given tiles for. Otherwise returns 'Just' with a tuple with the tiles
  given, and the new letterbag.
-}
exchangeLetters :: LetterBag -> [Tile] -> IO (Maybe ([Tile], LetterBag))
exchangeLetters (LetterBag tiles lettersLeft) exchanged =
  if (lettersLeft == 0) 
    then return Nothing
     else
      do
        shuffledBag <- shuffleBag intermediateBag
        return $ takeLetters shuffledBag numLettersGiven
  where
    numLettersGiven = length exchanged
    intermediateBag = LetterBag (exchanged ++ tiles) (lettersLeft + numLettersGiven)



  -- Shuffles the contents of a letter bag
shuffleBag :: LetterBag -> IO LetterBag
shuffleBag (LetterBag _ 0) =  return (LetterBag [] 0)
shuffleBag (LetterBag tiles size) = do
  shuffled <- shuffle tiles size
  return (LetterBag shuffled size)

  where
    -- Taken from http://www.haskell.org/haskellwiki/Random_shuffle
    shuffle :: [a] -> Int -> IO [a]
    shuffle xs bagSize = do
            ar <- newArray bagSize xs
            forM [1..bagSize] $ \i -> do
                j <- randomRIO (i,bagSize)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
      
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray size xs =  newListArray (1,size) xs

parseBag :: String -> Either ParseError [Tile]
parseBag contents = parse bagFile "Malformed letter bag file" contents
  where
    bagFile =
      do tiles <- many bagLine
         eof 
         return (concat tiles)

    bagLine =
      do tiles <- try (letterTiles) <|> blankTiles
         return tiles

    letterTiles =
      do 
         tileLetter <- letter
         space
         value <- many digit
         space
         distribution <- many digit
         newline
         return $ replicate (read distribution) (Letter (toUpper tileLetter) (read value))

    blankTiles =
      do 
        char <- char '_'
        space
        distribution <- many digit
        newline
        return $ replicate (read distribution) (Blank Nothing)