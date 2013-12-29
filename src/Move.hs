module Move where

  import ScrabbleError
  import FormedWord
  import Control.Monad
  import Data.Maybe

  makeBoardMove :: Game -> Player -> Map Pos Tile -> Either ScrabbleError (Player, Game, FormedWords)
  makeBoardMove game player placed = 
    formedWords >>=
      (wordStrings formedWords) >>=
        \(overallScore, _) ->



    

    where
      placedTiles = (map 2nd placed)
      currentBoard = board game
      moveNo = moveNumber game
      dict = dictionary game
      letterBag = bag game

      tryNewBoard = foldM (\(board, (pos, tile)) -> placeTile board tile pos) currentBoard placed
      tryNewBag = if (bagSize >= 7) then takeLetters letterBag 7 else takeLetters letterBag bagSize

      formedWords = if (moveNo == 1)
       then wordFormedFirstMove currentBoard placed 
       else wordsFormedMidGame currentBoard placed 

  newPlayerAndBag :: Player -> LetterBag -> [Tile] -> (LetterBag, Player)

  scoresIfWordsLegal :: Dictionary -> FormedWords -> Either WordsNotInDictionary (Int, [(String, Int)])
  scoresIfWordsLegal dict formedWords = 
    let wordStrings = wordStrings formedWords
      in case invalidWords dict wordStrings of
        (x:xs) -> Left $ WordsNotInDictionary x:xs
        otherwise -> Right $ wordsWithScores formedWords
