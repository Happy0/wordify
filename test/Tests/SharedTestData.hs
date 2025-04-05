module Tests.SharedTestData where

import Data.Char
import qualified Data.Map as M
import Data.Maybe
import qualified System.FilePath as F
import Wordify.Rules.Dictionary (Dictionary, makeDictionary)
import Wordify.Rules.Game
import Wordify.Rules.LetterBag
import Wordify.Rules.Player
import Wordify.Rules.Pos
import Wordify.Rules.Pos.Internal
import Wordify.Rules.ScrabbleError (ScrabbleError)
import Wordify.Rules.Square
import Wordify.Rules.Tile

horizontalPositions = catMaybes $ map posAt $ iterate (\(x, y) -> (x + 1, y)) (5, 7)

horizontalSquares = [Normal $ Just (Letter "H" 4), Normal $ Just (Letter "E" 1), DoubleLetter $ Just (Letter "L" 1), Normal $ Just (Letter "L" 1), DoubleLetter $ Just (Letter "O" 1)]

rogueLeft = (Pos 3 7 "C7", DoubleLetter $ Just (Letter "X" 2))

rogueRight = (Pos 11 7 "K7", Normal $ Just (Letter "Z" 2))

horizontals = zip horizontalPositions horizontalSquares

verticalPositions = catMaybes $ map posAt $ iterate (\(x, y) -> (x, y + 1)) (7, 5)

verticalSquares = [Normal $ Just (Letter "T" 1), Normal $ Just (Letter "E" 1), DoubleLetter $ Just (Letter "L" 1), Normal $ Just (Letter "L" 1), DoubleLetter $ Just (Letter "Y" 4)]

rogueAbove = (Pos 7 3 "G3", DoubleLetter $ Just (Letter "X" 2))

rogueBelow = (Pos 7 11 "G11", Normal $ Just (Letter "Z" 2))

verticals = zip verticalPositions verticalSquares

testDictionary :: IO (Either ScrabbleError Dictionary)
testDictionary = makeDictionary $ "Config" ++ [F.pathSeparator] ++ "engSet" ++ [F.pathSeparator] ++ "en.txt"

letterValues :: M.Map String Int
letterValues = M.fromList $ [("A", 1), ("B", 3), ("C", 3), ("D", 2), ("E", 1), ("F", 4), ("G", 2), ("H", 4), ("I", 1), ("J", 8), ("K", 5), ("L", 1), ("M", 3), ("N", 1), ("O", 1), ("P", 3), ("Q", 10), ("R", 1), ("S", 1), ("T", 1), ("U", 1), ("V", 4), ("W", 4), ("X", 8), ("Y", 4), ("Z", 10)]

toTilePlaced :: Char -> Tile
toTilePlaced char
  | isLower char = Blank $ Just ([toUpper char])
  | otherwise = toTileBag char

placeMap :: String -> Direction -> (Int, Int) -> M.Map Pos Tile
placeMap letters direction pos = M.fromList $ zip positions tiles
  where
    positions =
      case direction of
        Horizontal -> catMaybes $ takeWhile isJust <$> map posAt $ iterate (\(x, y) -> (x + 1, y)) pos
        Vertical -> catMaybes $ takeWhile isJust <$> map posAt $ iterate (\(x, y) -> (x, y + 1)) pos

    tiles = map toTilePlaced letters

toTileBag :: Char -> Tile
toTileBag lettr =
  case lettr of
    '_' -> Blank Nothing
    x -> Letter [x] $ M.findWithDefault 0 [x] letterValues

setupGame :: LetterBag -> IO (Either ScrabbleError Game)
setupGame bag =
  do
    dict <- testDictionary
    return $ resultGame bag dict
  where
    resultGame bag dict =
      do
        dc <- dict
        let [player1, player2, player3, player4] = map makePlayer ["a", "b", "c", "d"]
        makeGame (player1, player2, Just (player3, Just player4)) bag dc

isValid :: Either a b -> Bool
isValid (Right _) = True
isValid _ = False