module LetterBag (makeBag, lettersLeft, shuffleBag) where

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

data LetterBag = LetterBag { tiles :: [Tile],  lettersLeft :: Int } deriving Show

makeBag :: FilePath -> IO (Either ScrabbleError (LetterBag))
makeBag path =
	do
		fileContents <- Exc.try (readFile path) :: IO (Either Exc.IOException String)
		-- To do: Try to use 'either' function rather than case statements
		case fileContents of 
			Left e ->
				return ( Left (LetterBagFileNotFound path))

			Right str -> do
				  let tiles = parseBag str

				  case tiles of 
					Left e -> 
						 return (Left (MalformedLetterBagFile path))
					Right list -> do
						 let letterBag = LetterBag list (length list)
						 shuffledBag <- shuffleBag letterBag
						 return (Right shuffledBag)


-- Shuffles a letter bag
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
			do tiles <- try (letterTile) <|> blankTile
			   return tiles

		letterTile =
			do 
			   tileLetter <- anyChar
			   space
			   value <- many digit
			   space
			   distribution <- many digit
			   newline
			   return (replicate (read distribution) $ (Letter tileLetter (read value)) )

		blankTile =
			do 
				char <- char '_'
				space
				distribution <- many digit
				newline
				return $ replicate (read distribution) (Blank Nothing)