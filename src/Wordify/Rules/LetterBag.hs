module Wordify.Rules.LetterBag
  ( LetterBag,
    validLetters,
    makeBag,
    tiles,
    bagFromTiles,
    makeBagUsingGenerator,
    takeLetters,
    exchangeLetters,
    shuffleBag,
    shuffleWithNewGenerator,
    bagSize,
    getGenerator,
  )
where

import qualified Control.Exception as Exc
import Control.Monad
import Control.Monad.ST
import Data.Array.IO
import Data.Array.ST
import Data.Char
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import Data.STRef
import qualified Data.Set as S
import System.IO
import System.Random
import Text.ParserCombinators.Parsec
import Wordify.Rules.LetterBag.Internal
import Wordify.Rules.ScrabbleError
import Wordify.Rules.Tile

-- |
--  Creates a letter bag from a file where each line contains a space delimited letter character, letter value, and letter distribution.
--  A blank letter is represented by a '_' character and has a disribution, but no value.
--
-- If successful, the letter bag is shuffled before it is returned.
makeBag :: FilePath -> IO (Either ScrabbleError LetterBag)
makeBag path = do
  ioOutcome <- Exc.try $ withFile path ReadMode (hGetContents >=> parseBagString path) :: IO (Either Exc.IOException (Either ScrabbleError LetterBag))
  case ioOutcome of
    Left _ -> return $ Left (LetterBagFileNotOpenable path)
    Right x -> return $ fmap shuffleBag x

parseBagString :: String -> String -> IO (Either ScrabbleError LetterBag)
parseBagString path bagString =
  let parseResult = parseBag bagString
   in case parseResult of
        Left _ -> return $ Left (MalformedLetterBagFile path)
        Right parsedTiles ->
          do
            gen <- newStdGen
            return $ Right (LetterBag parsedTiles (length parsedTiles) gen (bagLetters parsedTiles))

-- |
--  Creates a letter bag from a list of tiles. The order of the tiles is retained in the resulting letter bag.
--
--  This function is effectful as it is necessary to create a stdGen for list to allow
--  it to be shuffled using this generator in the future.
bagFromTiles :: [Tile] -> IO LetterBag
bagFromTiles bagTiles =
  do
    generator <- newStdGen
    return $ LetterBag bagTiles (length bagTiles) generator (bagLetters bagTiles)

-- |
--    Helper function to construct a LetterBag. Maps the valid letters in a letter bag
--    to the tile representing that letter on the board.
bagLetters :: [Tile] -> M.Map String Tile
bagLetters tiles =
  let maybeLetters = Mb.mapMaybe pairIfTileHasLetter tiles
   in M.fromList maybeLetters
  where
    pairIfTileHasLetter :: Tile -> Maybe (String, Tile)
    pairIfTileHasLetter tile =
      case tileString tile of
        Just lettr -> Just (lettr, tile)
        _ -> Nothing

-- |
--  Takes 'n' numbers from a letter bag, yielding 'Nothing'
--  if there is not enough tiles left in the bag or a 'Just'
--  tuple where the left value is the taken tiles, and the right
--  value is the new bag.
takeLetters :: LetterBag -> Int -> Maybe ([Tile], LetterBag)
takeLetters (LetterBag bagTiles lettersLeft gen validLetters) numTake =
  if newNumLetters < 0
    then Nothing
    else Just (taken, LetterBag newLetters newNumLetters gen validLetters)
  where
    newNumLetters = lettersLeft - numTake
    (taken, newLetters) = splitAt numTake bagTiles

-- |
--  Exchanges given tiles for the same number of tiles from the bag.
--  The exchanged letters are added to the bag, the bag is then shuffled,
--  and then the same number of tiles as exchanged are drawn from the bag.
--
--  Returns 'Nothing' if there are not enough letters in the bag to exchange
--  the given tiles for. Otherwise returns 'Just' with a tuple with the tiles
--  given, and the new letterbag.
exchangeLetters :: LetterBag -> [Tile] -> (Maybe ([Tile], LetterBag))
exchangeLetters (LetterBag bagTiles lettersLeft gen validLetters) exchanged =
  if lettersLeft == 0 then Nothing else takeLetters (shuffleBag intermediateBag) numLettersGiven
  where
    numLettersGiven = length exchanged
    intermediateBag = LetterBag (exchanged ++ bagTiles) (lettersLeft + numLettersGiven) gen validLetters

-- |
--  Shuffles the contents of a letter bag. The bag is shuffled using the random generator which was created
--  while constructing the bag.
--
-- This function should not be used when creating an additional game with a new letter bag as
-- the same seed value will be shared across games (meaning tiles will come out of the bag in
-- the same order.) When constructing an additional game, use shuffleWithNewGenerator.
shuffleBag :: LetterBag -> LetterBag
shuffleBag (LetterBag _ 0 gen validLetters) = LetterBag [] 0 gen validLetters
shuffleBag (LetterBag bagTiles size gen validLetters) =
  let (newTiles, newGenerator) = shuffle bagTiles gen size
   in (LetterBag newTiles size newGenerator validLetters)
  where
    -- Taken from http://www.haskell.org/haskellwiki/Random_shuffle
    shuffle :: [a] -> StdGen -> Int -> ([a], StdGen)
    shuffle xs randomGen listLength =
      runST
        ( do
            g <- newSTRef randomGen
            let randomRST lohi = do
                  (a, s') <- liftM (randomR lohi) (readSTRef g)
                  writeSTRef g s'
                  return a
            ar <- newArr n xs
            xs' <- forM [1 .. n] $ \i -> do
              j <- randomRST (i, n)
              vi <- readArray ar i
              vj <- readArray ar j
              writeArray ar j vi
              return vj
            gen' <- readSTRef g
            return (xs', gen')
        )
      where
        n = listLength
        newArr :: Int -> [a] -> ST s (STArray s Int a)
        newArr z zs = newListArray (1, z) zs

-- |
--  Creates a letter bag using a list of tiles, and a generator which should be used when shuffling the bag.
--  This function allows a game to be stepped through from the beginning where the moves and original generator were
--  recorded, with any shuffling yielding the same bag as in the original game.
makeBagUsingGenerator :: [Tile] -> StdGen -> LetterBag
makeBagUsingGenerator bagTiles randomGenerator = LetterBag bagTiles (length bagTiles) randomGenerator (bagLetters bagTiles)

-- |
--  Get the letter bag's current generator, which will be used to shuffle the contents of the bag in the next exchange
--  or shuffle. If taken at the start of the game, with the original list of tiles in the bag in order, the game moves
--  may be replayed in order with the original results of any shuffle retained.
getGenerator :: LetterBag -> StdGen
getGenerator = generator

-- |
--  Shuffles a letter bag using a new random generator. This function should be used when spawning a new game using
--  a letter bag with all the tiles remaining so that letter bags are unique between game instances.
shuffleWithNewGenerator :: LetterBag -> IO LetterBag
shuffleWithNewGenerator letterBag = fmap (\newGen -> shuffleBag $ letterBag {generator = newGen}) newStdGen

parseBag :: String -> Either ParseError [Tile]
parseBag contents = parse bagFile "Malformed letter bag file" contents
  where
    bagFile =
      do
        bagTiles <- many bagLine
        eof
        let flattenedTiles = concat bagTiles
        return $ flattenedTiles

    bagLine = try (letterTiles) <|> blankTiles

    letterTiles =
      do
        tileCharacter <- many letter
        _ <- space
        value <- many digit
        _ <- space
        distribution <- many digit
        _ <- newline
        return $ replicate (read distribution) (Letter (map toUpper tileCharacter) (read value))

    blankTiles =
      do
        _ <- char '_'
        _ <- space
        distribution <- many digit
        _ <- newline
        return $ replicate (read distribution) (Blank Nothing)
