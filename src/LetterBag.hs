module LetterBag (
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
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

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
      Right tiles ->
        do 
          generator <- newStdGen
          return $ Right (LetterBag tiles (length tiles) generator)

{-
  Creates a letter bag from a list of tiles. The order of the tiles is retained in the resulting letter bag.

  This function is effectful as it is necessary to create a stdGen for list to allow
  it to be shuffled using this generator in the future.
-}
bagFromTiles :: [Tile] -> IO LetterBag
bagFromTiles tiles = newStdGen >>= return . LetterBag tiles (length tiles)

{-
  Takes 'n' numbers from a letter bag, yielding 'Nothing'
  if there is not enough tiles left in the bag or a 'Just'
  tuple where the left value is the taken tiles, and the right
  value is the new bag.
-}
takeLetters :: LetterBag -> Int -> Maybe ([Tile], LetterBag)
takeLetters (LetterBag tiles lettersLeft generator) numTake =
  if (newNumLetters < 0) then Nothing
   else Just (taken, LetterBag newLetters newNumLetters generator)
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
exchangeLetters :: LetterBag -> [Tile] -> (Maybe ([Tile], LetterBag))
exchangeLetters (LetterBag tiles lettersLeft generator) exchanged =
  if (lettersLeft == 0) then Nothing else takeLetters (shuffleBag intermediateBag) numLettersGiven
    where
      numLettersGiven = length exchanged
      intermediateBag = LetterBag (exchanged ++ tiles) (lettersLeft + numLettersGiven) generator

{-
  Shuffles the contents of a letter bag. The bag is shuffled using the random generator which was created
  while constructing the bag.

 This function should not be used when creating an additional game with a new letter bag as
 the same seed value will be shared across games (meaning tiles will come out of the bag in
 the same order.) When constructing an additional game, use shuffleWithNewGenerator.
-}
shuffleBag :: LetterBag -> LetterBag
shuffleBag (LetterBag _ 0 gen) =  LetterBag [] 0 gen
shuffleBag (LetterBag tiles size generator) =
  let (newTiles, newGenerator) = shuffle tiles generator size
  in (LetterBag newTiles size newGenerator)

  where
    -- Taken from http://www.haskell.org/haskellwiki/Random_shuffle
    shuffle :: [a] -> StdGen -> Int -> ([a],StdGen)
    shuffle xs gen listLength = runST (do
            g <- newSTRef gen
            let randomRST lohi = do
                  (a,s') <- liftM (randomR lohi) (readSTRef g)
                  writeSTRef g s'
                  return a
            ar <- newArray n xs
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
        newArray :: Int -> [a] -> ST s (STArray s Int a)
        newArray n xs =  newListArray (1,n) xs

{- 
  Creates a letter bag using a list of tiles, and a generator which should be used when shuffling the bag.
  This function allows a game to be stepped through from the beginning where the moves and original generator were
  recorded, with any shuffling yielding the same bag as in the original game.
-}
makeBagUsingGenerator :: [Tile] -> StdGen -> LetterBag
makeBagUsingGenerator tiles generator = LetterBag tiles (length tiles) generator

{-
  Get the letter bag's current generator, which will be used to shuffle the contents of the bag in the next exchange
  or shuffle. If taken at the start of the game, with the original list of tiles in the bag in order, the game moves
  may be replayed in order with the original results of any shuffle retained.
-}
getGenerator :: LetterBag -> StdGen
getGenerator = generator

{-
  Shuffles a letter bag using a new random generator. This function should be used when spawning a new game using
  a letter bag with all the tiles remaining so that letter bags are unique between game instances.
-}
shuffleWithNewGenerator :: LetterBag -> IO LetterBag
shuffleWithNewGenerator (LetterBag tiles size generator) = 
  do
    newGenerator <- newStdGen
    let newBag = LetterBag tiles size newGenerator
    return $ shuffleBag newBag

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