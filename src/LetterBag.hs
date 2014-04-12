module LetterBag (LetterBag, makeBag, takeLetters, exchangeLetters, shuffleBag, bagSize, tiles, bagFromTiles) where

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
import LetterBag.Internal
import System.IO
import Control.DeepSeq

{-
  Creates a letter bag from a file where each line contains a space delimited letter character, letter value, and letter distribution.
  A blank letter is represented by a '_' character and has no value.

  Example file:

  _ 2
  E 1 12
  A 1 9
  I 1 9
  O 1 8
  N 1 6
  R 1 6
  T 1 6
  L 1 4
  S 1 4
  U 1 4
  D 2 4
  G 2 3
  B 3 2
  C 3 2
  M 3 2
  P 3 2
  F 4 2
  H 4 2
  V 4 2
  W 4 2
  Y 4 2
  K 5 1
  J 8 1
  X 8 1
  Q 10 1
  Z 10 1

 The letter bag is shuffled before it is returned.

-}
makeBag :: FilePath -> IO (Either ScrabbleError LetterBag)
makeBag path = do
 ioOutcome <- Exc.try $ withFile path ReadMode (hGetContents >=> parseBagString path) :: IO (Either Exc.IOException (Either ScrabbleError LetterBag))
 case ioOutcome of
  Left _ -> return $ Left (LetterBagFileNotOpenable path)
  Right x -> return x

parseBagString :: String -> String -> IO (Either ScrabbleError LetterBag)
parseBagString path bagString  = 
  let parseResult = parseBag bagString in 
    case parseResult of
      Left _ -> return $ Left (MalformedLetterBagFile path)
      Right letterBag -> shuffleBag letterBag >>= return . Right

{-
  Creates a letter bag from a list of tiles. The order of the tiles is retained in the resulting letter bag.
-}
bagFromTiles :: [Tile] -> LetterBag
bagFromTiles tiles = LetterBag tiles (length tiles)

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

parseBag :: String -> Either ParseError LetterBag
parseBag contents = parse bagFile "Malformed letter bag file" contents
  where
    bagFile =
      do tiles <- many bagLine
         eof
         let flattenedTiles = concat tiles
         return $ LetterBag flattenedTiles (length flattenedTiles)

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