module Tests.FullGameTest where

    import Wordify.Rules.Dictionary
    import qualified Data.Map as M
    import Wordify.Rules.ScrabbleError
    import Wordify.Rules.LetterBag
    import Wordify.Rules.Pos
    import Wordify.Rules.Tile
    import Data.Maybe
    import Wordify.Rules.Move
    import Test.HUnit.Base
    import Wordify.Rules.Player
    import Wordify.Rules.Game
    import Wordify.Rules.Move
    import qualified Data.List.NonEmpty as NE
    import Tests.SharedTestData
    import Test.HUnit.Base
    import Data.Char
    import qualified Data.Sequence as Seq
    import qualified System.FilePath as F
    import Control.Monad   
    import Data.List

    data Direction = Horizontal | Vertical

    letterValues :: M.Map Char Int
    letterValues = M.fromList $ [('A', 1), ('B',3), ('C', 3), ('D', 2), ('E', 1), ('F',4),('G',2),('H',4),('I',1),('J',8),('K',5),('L',1) ,('M',3),('N',1),('O',1),('P',3),('Q',10),('R', 1), ('S',1), ('T',1),('U',1),('V',4),('W',4),('X',8),('Y',4),('Z',10)]

    testDictionary :: IO (Either ScrabbleError Dictionary)
    testDictionary = makeDictionary $ "Config" ++ [F.pathSeparator] ++ "engSet" ++ [F.pathSeparator] ++ "en.txt"

    letterBag :: IO LetterBag
    letterBag = bagFromTiles $ map toTileBag tilesAsLetters
        where
            tilesAsLetters = "JEARVINENVO_NILLEWBKONUIEUWEAZBDESIAPAEOOURGOCDSNIADOAACAR_RMYELTUTYTEREOSITNIRFGPHAQLHESOIITXFDMETG"

    setupGame :: IO (Either ScrabbleError Game)
    setupGame =
      do
        bag <- letterBag
        dict <- testDictionary
        return $ resultGame bag dict
      where
        resultGame bag dict =
          do
            dc <- dict
            let [player1, player2,player3,player4] = map makePlayer ["a","b","c","d"]
            makeGame (player1, player2, Just (player3, Just player4)) bag dc



    placeMap :: String -> Direction -> (Int, Int) -> M.Map Pos Tile 
    placeMap letters direction pos = M.fromList $ zip positions tiles
        where
            positions =
                case direction of
                    Horizontal -> catMaybes $ map posAt $ iterate (\(x,y) -> (x+1,y)) pos
                    Vertical -> catMaybes $ map posAt $ iterate (\(x,y) -> (x, y + 1)) pos

            tiles = map toTilePlaced letters

    toTileBag :: Char -> Tile
    toTileBag lettr = 
        case lettr of
            '_' -> Blank Nothing
            x -> Letter x $ M.findWithDefault 0 x letterValues

    toTilePlaced :: Char -> Tile
    toTilePlaced char
        | isLower char = Blank $ Just (toUpper char)
        | otherwise = toTileBag char

    moves :: [Move]
    moves = moveList

        where
            moveList = 
                map PlaceTiles [
                      placeMap "RAVINE" Horizontal (8,8)
                    , placeMap "OVEl" Vertical (12,9)
                    , placeMap "W" Vertical (9,7) `M.union` placeMap "KE" Vertical (9,9)
                    , placeMap "N" Horizontal (11,9)
                    , placeMap "B" Horizontal (13,7) `M.union` placeMap "D" Horizontal (13,9)
                    , placeMap "NAI" Horizontal (9,12)
                    , placeMap "B" Horizontal (11,11) `M.union` placeMap "LLE" Horizontal (13,11)
                    , placeMap "WEE" Vertical (10,13)
                    , placeMap "JA" Vertical (15,9) `M.union` placeMap "GERS" Vertical (15,12)
                    , placeMap "CANOPI" Horizontal (4,15) `M.union` placeMap "D" Horizontal (11,15)
                    , placeMap "SONI" Vertical (4,11)
                    , placeMap "AUDIO" Vertical (3,10)
                    , placeMap "RAZeR" Vertical (5,8)
                    , placeMap "MULEY" Vertical (2,6)
                    , placeMap "ROOTY" Vertical (3,2)
                    , placeMap "ETUIS" Vertical (14,4)
                    , placeMap "RACING" Vertical (1,10)
                    , placeMap "HATP" Vertical (11,4)
                    , placeMap "HAES" Vertical (12,2)
                    , placeMap "DOUX" Vertical (15,1)
                    , placeMap "GEM" Vertical (13,1)
                    , placeMap "Q" Horizontal (4,9) `M.union` placeMap "T" Horizontal (6,9)
                    , placeMap "IO" Vertical (6,13)
                    , placeMap "FIT" Vertical (10,2)
                ]


    playThroughTest :: Assertion
    playThroughTest = 
      do
        game <- setupGame
        assertBool "Could not initialise game for test " $ isValid game

        let Right testGame = game
        bag <- letterBag
        let moveTransitions = restoreGame testGame $ NE.fromList $ moves

        case moveTransitions of
            Left err ->
                assertFailure $ "Unable to play through test game, error was: " ++ show err
            Right transitions ->
                do
                    let finalTransition = NE.last transitions
                    assertBool "Expect the game to have ended" $ isFinalTransition finalTransition

                    let finalGame = newGame finalTransition

                    assertEqual "Unexpected number of moves" (length moves) (moveNumber finalGame)

                    assertEqual "Unexpected history for game" (History bag (Seq.fromList moves)) (history finalGame)

                    let finalBoard = board finalGame

                    let [finalPlayer1, finalPlayer2, finalPlayer3, finalPlayer4] = players finalGame

                    assertEqual "Unexpected final score for player 1" (189 - 5) (score finalPlayer1)
                    assertEqual "Unexpected remaining tiles for player 1" [Letter 'T' 1, Letter 'F' 4] (tilesOnRack finalPlayer1)

                    assertEqual "Unexpected remaining tiles for player 2" [Letter 'L' 1] (tilesOnRack finalPlayer2)
                    assertEqual "Unexpected final score for player 2" ( (136 + 50) - 1) (score finalPlayer2) -- This player scored a bingo word

                    assertEqual "Unexpected remaining tiles for player 3" [Letter 'E' 1] (tilesOnRack finalPlayer3)
                    assertEqual "Unexpected score for player 3" (110 - 1) (score finalPlayer3)

                    assertEqual "Unexpected remaing tiles for player 4" [] (tilesOnRack finalPlayer4)
                    assertEqual "Unexpected score for winning player" (154 + 1 + 5 + 1) (score finalPlayer4)


      where
        isFinalTransition trans =
         case trans of
            GameFinished _ _ _ -> True
            otherwise -> False

    gameEndsOnConsecutiveSkips :: Assertion
    gameEndsOnConsecutiveSkips = 
        do
          game <- setupGame
          -- 8 consecutive passes ends the game
          let skipMoves = NE.fromList $ replicate 8 Pass
          assertBool "Could not initialise game for test " $ isValid game

          let Right testGame = game 
          let transitions = restoreGame testGame skipMoves
          let lastGame = fmap NE.last transitions
          assertBool ("Unexpected failure when playing moves ") $ isValid lastGame

          let Right finalTrans = lastGame

          case finalTrans of
              GameFinished _ _ _ ->  assertEqual "Unexpected move number" (moveNumber (newGame finalTrans)) 8 
              otherwise -> assertFailure "Unexpected end state. Expected 'Game finished' "

    gameDoesNotEndOnNonConsecutiveSkips :: Assertion
    gameDoesNotEndOnNonConsecutiveSkips =
      do
        game <- setupGame
        assertBool "Could not initialise game for test " $ isValid game
        let Right testGame = game 
        let movesWithSkips = take 10 $ concat $ intersperse (replicate 4 Pass) $ splitEvery 4 moves
        let transitions = restoreGame testGame $ NE.fromList movesWithSkips

        assertBool "Unexpected error making moves" $ isValid transitions

        let Right gameTransitions = transitions
        let lastGame = newGame $ NE.last gameTransitions

        assertEqual "Expected game to still be in progress" InProgress (gameStatus lastGame)
        assertEqual "Unexpected player's move" ((10 `mod` 4) + 1) (playerNumber lastGame)
        assertEqual "Unexpected current player" (fmap fst (optionalPlayers lastGame)) ( Just (currentPlayer lastGame))

      where
        splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n) 

    exchangeMoveExchangesLetters :: Assertion
    exchangeMoveExchangesLetters = 
        do
            game <- setupGame
            assertBool "Could not initialise game for test " $ isValid game
            let Right testGame = game 
            let firstPlayer = player1 testGame
            let playerTiles = tilesOnRack firstPlayer
            let move = Exchange playerTiles
            let outcome = makeMove testGame move

            assertBool ("Expected move to be successful. ") $ isValid outcome
            let Right transition = outcome

            let nextGame = newGame transition
            let newPlayer1 = player1 nextGame


            case transition of 
                ExchangeTransition game playerBefore playerAfter -> 
                    do
                        assertEqual "playerBefore in the transition should be the player before making the move" (firstPlayer) playerBefore
                        assertEqual "playerAfter in the transition should be the player after making the move" (newPlayer1) playerAfter

            assertBool ("Player 1 should have new letters on their rack. Player 1 was: " ++ (show newPlayer1)) (not $ firstPlayer == newPlayer1)

            assertEqual "Game has transitioned to the next player " (currentPlayer nextGame)  (player2 testGame)
            
            assertBool "Player number and move number incremented" $ (playerNumber nextGame == 2) && (moveNumber nextGame) == 2

            let originalLetterBag = bag testGame
            let exchangedLetterBag = fmap snd (exchangeLetters originalLetterBag playerTiles)

            assertEqual "The letter bag for the game transition is as expected " exchangedLetterBag (Just $ bag nextGame)

    playerInMoveTransitionIsAsExpected :: Assertion
    playerInMoveTransitionIsAsExpected =
        do
            game <- setupGame
            assertBool "Could not initialise game for test " $ isValid game
            let Right testGame = game

            let move = head moves
            let outcome = makeMove testGame move

            case outcome of
                Right (MoveTransition player game formedwords) ->
                    do
                        assertEqual "Player should be the new state of the player in the game " (player1 game) player
                otherwise -> assertFailure "Failed to set up test correctly."





