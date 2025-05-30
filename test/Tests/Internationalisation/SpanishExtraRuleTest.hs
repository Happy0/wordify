
module Tests.Internationalisation.SpanishExtraRuleTest where
    import qualified System.FilePath as F
    import Wordify.Rules.ScrabbleError (ScrabbleError)
    import Wordify.Rules.Dictionary (Dictionary, makeDictionary)
    import Wordify.Rules.LetterBag (LetterBag, bagFromTiles)
    import Wordify.Rules.Tile (Tile(..))
    import Wordify.Rules.Game (Game, makeGame)
    import Wordify.Rules.Player (makePlayer)
    import Test.HUnit (Assertion, assertBool, assertEqual, Assertable)
    import Test.HUnit.Base (assertFailure)
    import Wordify.Rules.Move (GameTransition(..), Move (..), makeMove, newGame)
    import qualified Data.Map as M
    import Wordify.Rules.Pos (rightPositions, starPos, abovePositions, right, belowPositions, above, below)
    import Wordify.Rules.Extra.SpanishExtraRule (spanishGameExtraRules)
    import Wordify.Rules.Extra.ExtraRule (applyExtraRules, RuleExecutionError (..))
    import Control.Error (isLeft)
    import Control.Error.Util (isRight)
    import Data.Either (fromRight)
    import Wordify.Rules.Extra.ExtraRule (RuleApplicationResult(..), RuleApplicationsResult(..))
    import Data.Maybe (fromJust)
    import qualified Control.Arrow as A

    testSpanishDictionary :: IO (Either ScrabbleError Dictionary)
    testSpanishDictionary = makeDictionary $ "Config" ++ [F.pathSeparator] ++ "spanishSet" ++ [F.pathSeparator] ++ "dictionary.txt"

    testSpanishLetterBag :: IO LetterBag
    testSpanishLetterBag = bagFromTiles tiles
        where
            tiles = [Letter "CH" 5, Letter "U" 1,Letter "C" 3,Letter "U" 1,Letter "H" 4,Letter "A" 1,Letter "A" 1,Letter "E" 1,Letter "D" 2,
                    Letter "C" 3,Letter "E" 1,Letter "I" 1,Letter "A" 1,Letter "E" 1,Letter "C" 3,Letter "N" 1,
                    Letter "E" 1,Letter "A" 1,Letter "D" 2,Letter "D" 2,Letter "RR" 8,Letter "D" 2,Letter "N" 1,
                    Letter "P" 3,Letter "T" 1,Letter "O" 1,Letter "A" 1,Letter "S" 1,Letter "D" 2,Letter "S" 1,
                    Letter "E" 1,Letter "R" 1,Letter "I" 1,Letter "I" 1,Letter "A" 1,Letter "N" 1,Letter "A" 1,
                    Letter "T" 1,Letter "Ñ" 8,Letter "E" 1,Letter "S" 1,Letter "O" 1,Letter "T" 1,Letter "L" 1,
                    Letter "O" 1,Letter "LL" 8,Letter "I" 1,Letter "N" 1,Letter "S" 1,Letter "P" 3,Letter "E" 1,
                    Letter "B" 3,Letter "B" 3,Letter "E" 1,Letter "A" 1,Letter "A" 1,Letter "C" 3,Letter "Z" 10,Letter "E" 1,
                    Letter "U" 1,Letter "E" 1,Letter "E" 1,Letter "R" 1,Letter "M" 3,Letter "O" 1,Letter "L" 1,Letter "U" 1,
                    Letter "R" 1,Letter "S" 1,Letter "A" 1,Letter "I" 1,Letter "I" 1,
                    Letter "L" 1,Letter "G" 2,Letter "O" 1,Letter "A" 1,Letter "R" 1,Letter "T" 1,Letter "J" 8,
                    Letter "O" 1,Letter "F" 4,Blank Nothing,Letter "Y" 4,Letter "N" 1,Letter "L" 1,Letter "O" 1,
                    Letter "X" 8,Letter "E" 1,Letter "O" 1,Letter "R" 1,Letter "M" 3,Letter "S" 1,Letter "G" 2,Letter "U" 1,
                    Letter "H" 4,Letter "V" 4,Letter "A" 1,Letter "Q" 5,Letter "O" 1,Blank Nothing]

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

    testCannotPlayCandHOnOwn :: Assertion
    testCannotPlayCandHOnOwn = do
        gameResult <- testGame
        case gameResult of
            Left err -> assertFailure $ "Failed to set up game: " ++ show err
            Right game -> do
                let positions = rightPositions starPos 4
                let tiles = [Letter "C" 3, Letter "H" 4, Letter "A" 1]
                let moveMap = M.fromList $ zip positions tiles
                let move = PlaceTiles moveMap
                let result = makeMove game move

                case result of
                    Left err -> assertFailure $ "Initial game transition failed before applying extra rules: " ++ show err
                    Right moveTransition@MoveTransition {} -> do
                        let result = applyExtraRules moveTransition spanishGameExtraRules
                        assertBool "Expected rule to fail " (isLeft result)

                        case result of
                            Left (RuleExecutionError code description) -> do
                                assertEqual "Expected InvalidConsecutiveTiles as error code" "InvalidConsecutiveTiles" code
                                assertEqual "Expected correct error description" "Cannot place C and H consecutively" description
                            otherwise -> return ()

                    Right _ -> assertFailure "Unexpected move transition type"

    testCanPlayCHtile :: Assertion
    testCanPlayCHtile = do
        gameResult <- testGame
        case gameResult of
            Left err -> assertFailure $ "Failed to set up game: " ++ show err
            Right game -> do
                let positions = rightPositions starPos 4
                let tiles = [Letter "CH" 5, Letter "A" 1]
                let moveMap = M.fromList $ zip positions tiles
                let move = PlaceTiles moveMap

                let playerTwoMovePositions = reverse (drop 1 (fromJust (flip belowPositions 4 <$> right starPos)))
                let playerTwoMoveLetters = [Letter "C" 3, Letter "A" 1, Letter "D" 2]
                let moveMap = M.fromList $ zip playerTwoMovePositions playerTwoMoveLetters
                let move2 = PlaceTiles moveMap

                let result = makeMove game move
                case result of
                    Left err -> assertFailure $ "Initial game transition failed before applying extra rules: " ++ show err
                    Right moveTransition@MoveTransition {} -> do
                        let result = applyExtraRules moveTransition spanishGameExtraRules
                        assertBool "Expected rule to pass validation " (isRight result)
                        let Right (RuleApplicationsResult gameTransition _) = result
                        let secondMove = makeMove (newGame gameTransition) move2

                        case secondMove of
                            Left err -> assertFailure $ "Expected second move success. Error was " ++ show err
                            Right _ -> do
                                let result2 = applyExtraRules moveTransition spanishGameExtraRules
                                assertBool "Expected rule to pass validation " (isRight result2)

                    Right _ -> assertFailure "Unexpected move transition type"

    testCannotPlayCNextToExistingH :: Assertion
    testCannotPlayCNextToExistingH = do
        gameResult <- testGame
        case gameResult of
            Left err -> assertFailure $ "Failed to set up game: " ++ show err
            Right game -> do
                let positions = rightPositions starPos 2
                let tiles = [Letter "H" 4, Letter "A" 1]
                let moveMap = M.fromList $ zip positions tiles
                let move = PlaceTiles moveMap
                let result = A.left show (makeMove game move)

                let validated = result >>= A.left show <$> flip applyExtraRules spanishGameExtraRules
                assertBool "Should pass extra rule in first move" (isRight validated)

                let positionsMove2 = [above starPos, below starPos]
                let placedLettersMove2 = [Letter "C" 3, Letter "A" 1]
                let moveMap2 = M.fromList $ zip positionsMove2 placedLettersMove2
                let move2 = PlaceTiles moveMap
                let result2 = A.left show (makeMove game move2)

                assertBool "Move should succeed prior to extra rule" (isRight result2)

                let validated = result2 >>= A.left show <$> flip applyExtraRules spanishGameExtraRules
                assertBool "Should not pass extra validation on second move" (isRight validated)

                case validated of 
                    Left description -> assertEqual "Description should be as expected" "Cannot place C and H consecutively" description
                    Right _ -> return ()



