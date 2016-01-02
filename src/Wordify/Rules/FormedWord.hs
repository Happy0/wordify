module Wordify.Rules.FormedWord (FormedWords,
                                 FormedWord,
                                 PlacedSquares,
                                 allWords,
                                 mainWord,
                                 adjacentWords,
                                 playerPlaced,
                                 playerPlacedMap,
                                 scoreWord,
                                 overallScore,
                                 bingoBonusApplied,
                                 prettyPrintIntersections,
                                 makeString,
                                 wordStrings,
                                 wordsWithScores,
                                 wordsFormedMidGame,
                                 wordFormedFirstMove) where

  import Wordify.Rules.Pos
  import Wordify.Rules.Square
  import Wordify.Rules.Tile
  import Wordify.Rules.Board
  import Wordify.Rules.ScrabbleError
  import Data.Sequence as Seq
  import Data.Map as Map
  import Control.Applicative
  import Control.Monad
  import Data.Foldable as Foldable
  import qualified Data.Maybe as M
  import qualified Data.List.Split as S
  import Data.Char
  import Data.Functor

  data FormedWords =  FirstWord FormedWord  | FormedWords {
                                              main :: FormedWord
                                              , otherWords :: [FormedWord]
                                              , placed :: PlacedSquares
                                            } deriving (Show, Eq)
                                  
  type FormedWord = Seq (Pos, Square)
  type PlacedSquares = Map Pos Square

  {- |
    Pretty prints the places a given formed word intersects with letters that were already on the board
    using brackets. E.g. T(HI)S would denote that the player placed a 'T' and an 'S' on to the board, using
    the already placed word 'HI' to form the new word 'THIS'.
  -}
  prettyPrintIntersections :: PlacedSquares -> FormedWord -> String
  prettyPrintIntersections placed formedWord = denotePassThroughs placed $ Foldable.toList formedWord
    where
        denotePassThroughs :: PlacedSquares -> [(Pos, Square)] -> String
        denotePassThroughs placed formed =
          let breaks = brokenSquaresToChars $ S.split (splitter placed) formed
          in case breaks of
            (part:parts) -> part ++ (Prelude.concat $ Prelude.zipWith (++) (cycle ["(",")"]) parts)
            [] -> ""

        squareToChar :: Square -> Char
        squareToChar sq = maybe '_' id $ tileIfOccupied sq >>= printLetter

        -- Splits whenever we encounter a series of squares that the player's word passes through
        -- on the board
        splitter :: PlacedSquares -> S.Splitter (Pos, Square)
        splitter placed = S.condense $ S.whenElt (flip (Map.notMember . fst) placed)

        brokenSquaresToChars :: [[(Pos, Square)]] -> [[Char]]
        brokenSquaresToChars brokenSquares = (Prelude.map . Prelude.map) (squareToChar . snd) brokenSquares

  {- |
    Scores an individual word. 

    Note: overallscore should be used to obtain the overall score as it takes into account any bingo bonuses.
    
  -}
  scoreWord :: PlacedSquares -> FormedWord -> Int
  scoreWord played formed = 
    let (notAlreadyPlaced, onBoardAlready) = partitionPlaced played formed
    in scoreSquares onBoardAlready notAlreadyPlaced
      where
        partitionPlaced placed formed = (mapTuple . fmap) snd $ Seq.partition (\(pos, _) -> Map.member pos placed) formed

        mapTuple :: (a -> b) -> (a, a) -> (b, b)
        mapTuple f (a1, a2) = (f a1, f a2)

  {- |
    Calculates the overall score of the play.

    If a player managed to place all 7 of their letters, then they receive a bingo bonus of 50 points.
  -}
  overallScore :: FormedWords -> Int
  overallScore formedWords =
    let wordsScore = Prelude.sum $ Prelude.map (scoreWord placed) $ allWords formedWords
    in case (Prelude.length $ keys $ placed) of
      7 -> wordsScore + 50
      _ -> wordsScore
      where
        placed = playerPlacedMap formedWords

  {-|
    All the words formed by a play.
  -}
  allWords :: FormedWords -> [FormedWord]
  allWords (FormedWords main adjacentWords _) = main :  adjacentWords
  allWords (FirstWord firstWord) = [firstWord]

  {- |
     Returns the word formed by the first move on the board. The word must cover
     the star tile, and be linear. Any blank tiles must be labeled.
   -}
  wordFormedFirstMove :: Board -> Map Pos Tile -> Either ScrabbleError FormedWords
  wordFormedFirstMove board tiles
    | starPos `Map.notMember` tiles = Left DoesNotCoverTheStarTile
    | otherwise = placedSquares board tiles >>= fmap (FirstWord . main) . wordsFormed board

  {- |
    Returns the words formed by the tiles played on the board. A played word
    must be connected to a tile already on the board (or intersect tiles on the board), 
    and be formed linearly. Any blank tiles must be labeled.
  -}
  wordsFormedMidGame :: Board -> Map Pos Tile -> Either ScrabbleError FormedWords
  wordsFormedMidGame board tiles = placedSquares board tiles >>=
   \squares -> wordsFormed board squares >>= \formed ->
    let FormedWords x xs _  = formed
    -- Check it connects to at least one other word on the board
    in if Seq.length x > Map.size squares || not (Prelude.null xs)
           then Right $ FormedWords x xs squares
            else Left DoesNotConnectWithWord

  {- |
    Returns the main word formed by the played tiles. The main word is
    the linear stretch of tiles formed by the tiles placed.
  -}
  mainWord :: FormedWords -> FormedWord
  mainWord (FirstWord word) = word
  mainWord formed = main formed

  {- |
    Returns the list of words which were adjacent to the main word formed. 
  -}
  adjacentWords :: FormedWords -> [FormedWord]
  adjacentWords (FirstWord _) = []
  adjacentWords formed = otherWords formed

  {- | 
    Returns the list of positions mapped to the squares that the player placed their tiles on.
  -}
  playerPlaced :: FormedWords -> [(Pos, Square)]
  playerPlaced (FirstWord word) = Foldable.toList word
  playerPlaced formed = Map.toList $ placed formed

  playerPlacedMap :: FormedWords -> Map Pos Square
  playerPlacedMap (FirstWord word) = Map.fromList $ Foldable.toList word
  playerPlacedMap formed = placed formed

  {- |
    Scores the words formed by the tiles placed. The first item in the tuple is the overall
    score, while the second item is the list of scores for all the words formed.
  -}
  wordsWithScores :: FormedWords -> (Int, [(String, Int)])
  wordsWithScores formedWords = (overallScore formedWords, fmap wordAndScore allFormedWords)
    where
      allFormedWords = allWords formedWords
      wordAndScore formedWord = (makeString formedWord, scoreWord (playerPlacedMap formedWords) formedWord)

  {- |
    Returns true if the player placed all 7 of their letters while forming these words, incurring a + 50 score bonus.
  -}
  bingoBonusApplied :: FormedWords -> Bool
  bingoBonusApplied formed = Prelude.length (playerPlaced formed) == 7
  
  {- |
    Returns the words formed by the play as strings.
  -}
  wordStrings :: FormedWords -> [String]
  wordStrings (FirstWord word) = [makeString word]
  wordStrings formed = Prelude.map makeString $ main formed : otherWords formed

  makeString :: FormedWord -> String
  makeString word = M.mapMaybe (\(_, sq) -> tileIfOccupied sq >>= tileLetter) $ Foldable.toList word

  {-
    Checks that the tiles can be placed, and if so returns a map of the squares at the placed positions.
    A tile may be placed if the square is not already occupied, and if it is not an unlabeled blank tile.
  -}
  placedSquares :: Board -> Map Pos Tile -> Either ScrabbleError (Map Pos Square)
  placedSquares board tiles = squares
      where
        squares = Map.fromList <$> sequence ((\ (pos, tile) -> 
          posTileIfNotBlank (pos, tile) >>= squareIfUnoccupied) <$> mapAsList)

        posTileIfNotBlank (pos,tile) = 
          if tile == Blank Nothing then Left (CannotPlaceBlankWithoutLetter pos) else Right (pos, tile)
        squareIfUnoccupied (pos,tile) = maybe (Left (PlacedTileOnOccupiedSquare pos tile)) (\sq ->
         Right (pos, putTileOn sq tile)) $ unoccupiedSquareAt board pos
        mapAsList = Map.toList tiles

  wordsFormed :: Board -> Map Pos Square -> Either ScrabbleError FormedWords
  wordsFormed board tiles
    | Map.null tiles = Left NoTilesPlaced
    | otherwise = formedWords >>= \formed -> 
        case formed of
          x : xs -> Right $ FormedWords x xs tiles
          [] -> Left NoTilesPlaced
      where
        formedWords = maybe (Left $ MisplacedLetter maxPos) (\direction -> 
            middleFirstWord direction >>= (\middle -> 
                            let (midWord, _) = middle
                            in let mainLine = preceding direction minPos >< midWord >< after direction maxPos
                            in Right $ mainLine : adjacentToMain (swapDirection direction) ) ) getDirection

        preceding direction pos = case direction of
                                    Horizontal -> lettersLeft board pos
                                    Vertical -> lettersBelow board pos
        after direction pos =  case direction of
                                    Horizontal -> lettersRight board pos
                                    Vertical -> lettersAbove board pos

        (minPos, _) = Map.findMin tiles
        (maxPos, _) = Map.findMax tiles

        adjacentToMain direction = Prelude.filter (\word -> Seq.length word > 1) $ Prelude.map (\(pos, square) ->
         (preceding direction pos |> (pos, square)) >< after direction pos) placedList

        middleFirstWord direction =
         case placedList of 
              [x] -> Right (Seq.singleton x, minPos)
              (x:xs) -> 
                foldM (\(word, lastPos) (pos, square) -> 
                  if not $ stillOnPath lastPos pos direction
                   then Left $ MisplacedLetter pos
                    else 
                      if isDirectlyAfter lastPos pos direction then Right (word |> (pos, square), pos) else
                        let between = after direction lastPos in
                        if expectedLettersInbetween direction lastPos pos between
                         then Right ( word >< ( between |> (pos,square) ), pos)
                          else Left $ MisplacedLetter pos
                ) (Seq.singleton x, minPos ) xs
              [] -> Left NoTilesPlaced

        placedList = Map.toAscList tiles

        stillOnPath lastPos thisPos direction = staticDirectionGetter direction thisPos == staticDirectionGetter direction lastPos
        expectedLettersInbetween direction lastPos currentPos between =
         Seq.length between + 1 == movingDirectionGetter direction currentPos - movingDirectionGetter direction lastPos

        swapDirection direction = if direction == Horizontal then Vertical else Horizontal

        getDirection
          -- If only one tile is placed, we look for the first tile it connects with if any. If it connects with none, we return 'Nothing'
          | (minPos == maxPos) && (not (Seq.null (lettersLeft board minPos)) || not (Seq.null (lettersRight board minPos))) = Just Horizontal
          | (minPos == maxPos) && (not (Seq.null (lettersBelow board minPos)) || not (Seq.null (lettersAbove board minPos))) = Just Vertical
          | xPos minPos == xPos maxPos = Just Vertical
          | yPos minPos == yPos maxPos = Just Horizontal
          | otherwise = Nothing

        staticDirectionGetter direction pos = if direction == Horizontal then yPos pos else xPos pos

        movingDirectionGetter direction pos = if direction == Horizontal then xPos pos else yPos pos

        isDirectlyAfter pos nextPos direction = movingDirectionGetter direction nextPos == movingDirectionGetter direction pos + 1
