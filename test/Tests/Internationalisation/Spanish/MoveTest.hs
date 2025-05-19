module Tests.Internationalisation.Spanish.MoveTest (playSpanishMoveTest) where

    import Wordify.Rules.LetterBag
    import Wordify.Rules.ScrabbleError (ScrabbleError)
    import Wordify.Rules.Dictionary (Dictionary, makeDictionary)
    import qualified System.FilePath as F
    import Wordify.Rules.Game (Game, makeGame)
    import Test.HUnit (Assertion)
    import qualified Data.Map as M
    import Wordify.Rules.Move (GameTransition(..), makeMove, Move (PlaceTiles))
    import Test.HUnit.Base (assertFailure)
    import Wordify.Rules.Tile (Tile(Letter, Blank))
    import Wordify.Rules.Pos (starPos, rightPositions)
    import Wordify.Rules.Player(makePlayer)
    
    testSpanishDictionary :: IO (Either ScrabbleError Dictionary)
    testSpanishDictionary = makeDictionary $ "Config" ++ [F.pathSeparator] ++ "spanishSet" ++ [F.pathSeparator] ++ "dictionary.txt"

    testSpanishLetterBag :: IO LetterBag
    testSpanishLetterBag = bagFromTiles tiles
        where
            tiles = [Letter "O" 1,Letter "Ñ" 8,Letter "S" 1, Letter "A" 1, Letter "W" 8,Letter "J" 6,Letter "L" 1,Letter "D" 2,Letter "S" 1,Letter "L" 1,
                Letter "F" 4,Letter "U" 1,Letter "R" 1,Letter "L" 1,Letter "C" 2,Letter "I" 1,Letter "T" 1,
                Letter "B" 3,Letter "K" 8,Blank Nothing,Letter "Z" 10,Letter "C" 2,Letter "E" 1,Letter "V" 4,Letter "I" 1,
                Letter "U" 1,Letter "G" 2,Letter "A" 1,Letter "I" 1,Letter "M" 3,Letter "O" 1,Letter "H" 4,
                Letter "O" 1,Letter "M" 3,Letter "L" 1,Letter "Q" 8,Letter "S" 1,Letter "M" 3,Blank Nothing,Letter "T" 1,
                Letter "S" 1,Letter "X" 8,Letter "R" 1,Letter "E" 1,Letter "B" 3,Letter "E" 1,Letter "V" 4,Letter "O" 1,
                Letter "U" 1,Letter "R" 1,Letter "A" 1,Letter "A" 1,Letter "G" 2,Letter "R" 1,Letter "C" 2,Letter "E" 1,Letter "A" 1,
                Letter "A" 1,Letter "S" 1,Letter "B" 3,Letter "P" 3,Letter "O" 1,Letter "N" 1,Letter "I" 1,Letter "E" 1,Letter "A" 1,
                Letter "E" 1,Letter "Y" 4,Letter "A" 1,Letter "I" 1,Letter "O" 1,Letter "J" 6,
                Letter "A" 1,Letter "I" 1,Letter "E" 1,Letter "D" 2,Letter "LL" 8,Letter "C" 2,Letter "F" 4,Letter "O" 1,
                Letter "E" 1,Letter "N" 1,Letter "T" 1,Letter "D" 2,Letter "E" 1,Letter "E" 1,Letter "U" 1,Letter "T" 1,
                Letter "E" 1,Letter "U" 1,Letter "P" 3,Letter "N" 1,Letter "S" 1,Letter "O" 1,Letter "S" 1,Letter "N" 1,
                Letter "U" 1,Letter "RR" 8,Letter "A" 1,Letter "D" 2,Letter "N" 1,Letter "H" 4,Letter "A" 1]

    testGame :: IO (Either ScrabbleError Game)
    testGame = do
        letterBag <- testSpanishLetterBag
        dictionaryResult <- testSpanishDictionary

        case dictionaryResult of
            Left err -> return $ Left err
            Right dict -> pure $ setupGame letterBag dict

        where
            setupGame :: LetterBag -> Dictionary -> Either ScrabbleError Game
            setupGame bag dict = makeGame (player1, player2, Nothing) bag dict
                where
                    player1 = makePlayer "player 1"
                    player2 = makePlayer "player 2"

    playSpanishMoveTest :: Assertion
    playSpanishMoveTest = do
        gameResult <- testGame
        case gameResult of
            Left err -> assertFailure $ "Failed to set up game: " ++ show err
            Right game -> do
                let positions = rightPositions starPos 4
                let tiles = [Letter "A" 1, Letter "Ñ" 8, Letter "O" 1, Letter "S" 1]
                let moveMap = M.fromList $ zip positions tiles
                let move = PlaceTiles moveMap
                let result = makeMove game move

                case result of
                    Left err -> assertFailure $ "Failed to make move: " ++ show err
                    Right (MoveTransition _ _ _) -> return ()
                    Right _ -> assertFailure "Unexpected result type"