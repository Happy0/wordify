module FormedWord (FormedWords, FormedWord, wordsFormedMidGame, wordFormedFirstMove, wordStrings, wordsWithScores, mainWord, adjacentWords, playerPlaced) where

  import Pos
  import Square
  import Tile
  import Board
  import ScrabbleError
  import Data.Sequence as Seq
  import Data.Map as Map
  import Control.Applicative
  import Control.Monad
  import Data.Foldable as Foldable
  import Data.Maybe

  data FormedWords =  FirstWord FormedWord  | FormedWords {
                                              main :: FormedWord
                                              , otherWords :: [FormedWord]
                                              , placed :: Map Pos Square
                                            }
                                  
  type FormedWord = Seq (Pos, Square)
  data Direction = Horizontal | Vertical deriving Eq

  {- 
     Returns the word formed by the first move on the board. The word must cover
     the star tile, and be linear.
   -}
  wordFormedFirstMove :: Board -> Map Pos Tile -> Either ScrabbleError FormedWords
  wordFormedFirstMove board tiles = 
    if (starPos `Map.notMember` tiles) 
      then Left DoesNotIntersectCoverTheStarTile
      else placedSquares board tiles >>= 
        \squares -> (\formed -> FirstWord $ main formed) <$> wordsFormed board squares

  {- 
    Returns the words formed by the tiles played on the board. A played word
    must be connected to a tile already on the board (or intersect tiles on the board), 
    and be formed linearly.
  -}
  wordsFormedMidGame :: Board -> Map Pos Tile -> Either ScrabbleError FormedWords
  wordsFormedMidGame board tiles = placedSquares board tiles >>=
   \squares -> wordsFormed board squares >>= \formed ->
    let FormedWords x xs _  = formed
    -- Check it connects to at least one other word on the board
    in if Seq.length x > Map.size squares || not (Prelude.null xs)
           then Right $ FormedWords x xs squares
            else Left $ DoesNotConnectWithWord

  mainWord :: FormedWords -> FormedWord
  mainWord (FirstWord word) = word
  mainWord (FormedWords main otherWords placed) = main

  adjacentWords :: FormedWords -> [FormedWord]
  adjacentWords (FormedWords main otherWords _) = otherWords
  adjacentWords _ = []

  playerPlaced :: FormedWords -> [(Pos, Square)]
  playerPlaced (FirstWord word) = Foldable.toList word
  playerPlaced (FormedWords _ _ placed) = Map.toList placed

  wordsWithScores :: FormedWords -> (Int, [(String, Int)])
  wordsWithScores (FirstWord firstWord) =
   let score = scoreWord Seq.empty (fmap snd firstWord) in (score, [(makeString firstWord, score)])
  wordsWithScores (FormedWords mainWord otherWords placed) = (Prelude.sum scores, Prelude.zip strings scores)
    where
      allWords = mainWord : otherWords
      strings = Prelude.map makeString allWords
      scores = Prelude.map (\formedWord -> let (placed, alreadyPlaced) = partitionPlaced formedWord 
                                           in scoreWord (fmap snd alreadyPlaced) (fmap snd placed) ) allWords
      partitionPlaced formedWord = Seq.partition (\(pos, square) -> Map.member pos placed) formedWord

  {-
    Returns the words formed by the play as strings.
  -}
  wordStrings :: FormedWords -> [String]
  wordStrings (FirstWord word) = [makeString word]
  wordStrings (FormedWords mainWord otherWords _) = Prelude.map makeString (mainWord : otherWords)

  makeString :: FormedWord -> String
  makeString word = catMaybes $ Prelude.map (\((_, sq)) -> tileIfOccupied sq >>= tileLetter) $ Foldable.toList word

  {-
    Checks that the tiles can be placed, and if so turns a map of the squares at the placed positions.
    A tile may be placed if the square is not already occupied, and if it is not an unlabeled blank tile.
  -}
  placedSquares :: Board -> Map Pos Tile -> Either ScrabbleError (Map Pos Square)
  placedSquares board tiles = squares
      where
        squares = Map.fromList <$> (sequence $ (\(pos, tile) -> 
          posTileIfNotBlank (pos,tile) >>= squareIfUnoccupied) <$> mapAsList)

        posTileIfNotBlank (pos,tile) = 
          if (tile == Blank Nothing) then Left (CannotPlaceBlankWithoutLetter pos) else Right (pos, tile)
        squareIfUnoccupied (pos,tile) = maybe (Left  (PlacedTileOnOccupiedSquare pos tile)) (\sq ->
         (Right (pos, putTileOn sq tile))) $ unoccupiedSquareAt board pos
        mapAsList = Map.toList tiles

  wordsFormed :: Board -> Map Pos Square -> Either ScrabbleError FormedWords
  wordsFormed board tiles
    | Map.null tiles = Left NoTilesPlaced
    | otherwise = formedWords >>= \formedWords -> 
        case formedWords of
          x : xs -> Right $ FormedWords x xs tiles
          [] -> Left NoTilesPlaced
      where
        formedWords = maybe (Left $ MisplacedLetter maxPos lastTile) (\direction -> 
            middleFirstWord direction >>= (\middleFirstWord -> 
                            let (midWord, square) = middleFirstWord
                            in let mainWord = preceding direction minPos >< midWord >< after direction maxPos
                            in Right $ mainWord : adjacentWords (swapDirection direction) ) ) getDirection

        preceding direction pos = case direction of
                                    Horizontal -> lettersLeft board pos
                                    Vertical -> lettersBelow board pos
        after direction pos =  case direction of
                                    Horizontal -> lettersRight board pos
                                    Vertical -> lettersAbove board pos

        (minPos, firstTile) = Map.findMin tiles
        (maxPos, lastTile) = Map.findMax tiles

        adjacentWords direction = Prelude.filter (\word -> Seq.length word > 1) $ Prelude.map (\(pos, square) ->
         (preceding direction pos |> (pos, square)) >< after direction pos) placedList

        middleFirstWord direction =
         case placedList of 
              x:[] -> Right (Seq.singleton x, minPos)
              (x:xs) -> 
                foldM (\(word, lastPos) (pos, square) -> 
                  if (not $ stillOnPath lastPos pos direction)
                   then Left $ MisplacedLetter pos square
                    else 
                      if (isDirectlyAfter pos lastPos direction) then Right $ (word |> (pos, square), pos) else
                        let between = after direction lastPos in
                        if expectedLettersInbetween direction lastPos pos between
                         then Right $ ( word >< ( between |> (pos,square) ), pos)
                          else Left $ MisplacedLetter pos square
                ) (Seq.singleton x, minPos ) $ xs

        placedList = Map.toList tiles

        stillOnPath lastPos thisPos direction = (staticDirectionGetter direction thisPos) == staticDirectionGetter direction lastPos
        expectedLettersInbetween direction lastPos currentPos between =
         Seq.length between + 1 == movingDirectionGetter direction currentPos - movingDirectionGetter direction lastPos

        swapDirection direction = if direction == Horizontal then Vertical else Horizontal

        getDirection
          -- If only one tile is placed, we look for the first tile it connects with if any. If it connects with none, we return 'Nothing'
          | (minPos == maxPos) && not (Seq.null (lettersLeft board minPos))
            || not (Seq.null (lettersRight board minPos)) = Just Horizontal
          | (minPos == maxPos) && not (Seq.null (lettersBelow board minPos))
           || not (Seq.null (lettersAbove board minPos)) = Just Vertical
          | (xPos minPos) == (xPos maxPos) = Just Vertical
          | (yPos minPos) == (yPos maxPos) = Just Horizontal
          | otherwise = Nothing

        staticDirectionGetter direction pos = if direction == Horizontal then yPos pos else xPos pos
        movingDirectionGetter direction pos = if direction == Horizontal then xPos pos else yPos pos

        isDirectlyAfter pos nextPos direction = 
          (movingDirectionGetter direction nextPos) == (movingDirectionGetter direction pos) + 1