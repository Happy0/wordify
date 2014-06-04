module Wordify.Rules.LetterBag (
 LetterBag,
 makeBag,
 takeLetters,
 exchangeLetters,
 shuffleBag, bagSize,
 tiles,
 bagFromTiles, 
 makeBagUsingGenerator, 
 getGenerator, 
 shuffleWithNewGenerator) where

import Wordify.Rules.Tile
import System.Random
import Data.Array.IO
import Control.Monad
import qualified Control.Exception as Exc
import Wordify.Rules.ScrabbleError
import Text.ParserCombinators.Parsec
import Data.Char
import Wordify.Rules.LetterBag.Internal
import System.IO
import Data.Array.ST
import Control.Monad.ST
import Data.STRef

{- |
  Creates a letter bag from a file where each line contains a space delimited letter character, letter value, and letter distribution.
  A blank letter is represented by a '_' character and has a disribution, but no value.

 If successful, the letter bag is shuffled before it is returned.

-}
makeBag :: FilePath -> IO (Either ScrabbleError LetterBag)
makeBag path = do
 ioOutcome <- Exc.try $ withFile path ReadMode (hGetContents >=> parseBagString path) :: IO (Either Exc.IOException (Either ScrabbleError LetterBag))
 case ioOutcome of
  Left _ -> return $ Left (LetterBagFileNotOpenable path)
  Right x -> return $ fmap shuffleBag x

parseBagString :: String -> String -> IO (Either ScrabbleError LetterBag)
parseBagString path bagString  = 
  let parseResult = parseBag bagString in 
    case parseResult of
      Left _ -> return $ Left (MalformedLetterBagFile path)
      Right parsedTiles ->
        do 
          gen <- newStdGen
          return $ Right (LetterBag parsedTiles (length parsedTiles) gen)

{- |
  Creates a letter bag from a list of tiles. The order of the tiles is retained in the resulting letter bag.

  This function is effectful as it is necessary to create a stdGen for list to allow
  it to be shuffled using this generator in the future.
-}
bagFromTiles :: [Tile] -> IO LetterBag
bagFromTiles bagTiles = newStdGen >>= return . LetterBag bagTiles (length bagTiles)

{- |
  Takes 'n' numbers from a letter bag, yielding 'Nothing'
  if there is not enough tiles left in the bag or a 'Just'
  tuple where the left value is the taken tiles, and the right
  value is the new bag.
-}
takeLetters :: LetterBag -> Int -> Maybe ([Tile], LetterBag)
takeLetters (LetterBag bagTiles lettersLeft gen) numTake =
  if (newNumLetters < 0) then Nothing
   else Just (taken, LetterBag newLetters newNumLetters gen)
  where
    newNumLetters = lettersLeft - numTake
    (taken, newLetters) = splitAt numTake bagTiles

{- |
  Exchanges given tiles for the same number of tiles from the bag.
  The exchanged letters are added to the bag, the bag is then shuffled, 
  and then the same number of tiles as exchanged are drawn from the bag.

  Returns 'Nothing' if there are not enough letters in the bag to exchange
  the given tiles for. Otherwise returns 'Just' with a tuple with the tiles
  given, and the new letterbag.
-}
exchangeLetters :: LetterBag -> [Tile] -> (Maybe ([Tile], LetterBag))
exchangeLetters (LetterBag bagTiles lettersLeft gen) exchanged =
  if (lettersLeft == 0) then Nothing else takeLetters (shuffleBag intermediateBag) numLettersGiven
    where
      numLettersGiven = length exchanged
      intermediateBag = LetterBag (exchanged ++ bagTiles) (lettersLeft + numLettersGiven) gen

{- |
  Shuffles the contents of a letter bag. The bag is shuffled using the random generator which was created
  while constructing the bag.

 This function should not be used when creating an additional game with a new letter bag as
 the same seed value will be shared across games (meaning tiles will come out of the bag in
 the same order.) When constructing an additional game, use shuffleWithNewGenerator.
-}
shuffleBag :: LetterBag -> LetterBag
shuffleBag (LetterBag _ 0 gen) =  LetterBag [] 0 gen
shuffleBag (LetterBag bagTiles size gen) =
  let (newTiles, newGenerator) = shuffle bagTiles gen size
  in (LetterBag newTiles size newGenerator)

  where
    -- Taken from http://www.haskell.org/haskellwiki/Random_shuffle
    shuffle :: [a] -> StdGen -> Int -> ([a],StdGen)
    shuffle xs randomGen listLength = runST (do
            g <- newSTRef randomGen
            let randomRST lohi = do
                  (a,s') <- liftM (randomR lohi) (readSTRef g)
                  writeSTRef g s'
                  return a
            ar <- newArr n xs
            xs' <- forM [1..n] $ \i -> do
                    j <- randomRST (i,n)
                    vi <- readArray ar i
                    vj <- readArray ar j
                    writeArray ar j vi
                    return vj
            gen' <- readSTRef g
            return (xs',gen'))
      where
        n = listLength
        newArr :: Int -> [a] -> ST s (STArray s Int a)
        newArr z zs =  newListArray (1,z) zs

{- |
  Creates a letter bag using a list of tiles, and a generator which should be used when shuffling the bag.
  This function allows a game to be stepped through from the beginning where the moves and original generator were
  recorded, with any shuffling yielding the same bag as in the original game.
-}
makeBagUsingGenerator :: [Tile] -> StdGen -> LetterBag
makeBagUsingGenerator tiles randomGenerator = LetterBag tiles (length tiles) randomGenerator

{- |
  Get the letter bag's current generator, which will be used to shuffle the contents of the bag in the next exchange
  or shuffle. If taken at the start of the game, with the original list of tiles in the bag in order, the game moves
  may be replayed in order with the original results of any shuffle retained.
-}
getGenerator :: LetterBag -> StdGen
getGenerator = generator

{- |
  Shuffles a letter bag using a new random generator. This function should be used when spawning a new game using
  a letter bag with all the tiles remaining so that letter bags are unique between game instances.
-}
shuffleWithNewGenerator :: LetterBag -> IO LetterBag
shuffleWithNewGenerator letterBag = fmap (\newGen -> shuffleBag $ letterBag { generator = newGen }) newStdGen
 
parseBag :: String -> Either ParseError [Tile]
parseBag contents = parse bagFile "Malformed letter bag file" contents
  where
    bagFile =
      do tiles <- many bagLine
         eof
         let flattenedTiles = concat tiles
         return $ flattenedTiles

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
