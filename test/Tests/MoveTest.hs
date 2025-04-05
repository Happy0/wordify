module Tests.MoveTest (movePlayedWithEmptyBlankTile, movePlayedWithInvalidBlankTileAssignment) where

import Control.Error
import Data.Char
import qualified Data.Map as M
import qualified System.FilePath as F
import Test.HUnit (Assertion)
import Test.HUnit.Base
import Tests.SharedTestData (isValid, placeMap, setupGame, toTileBag)
import Wordify.Rules.Dictionary
import Wordify.Rules.Game
import Wordify.Rules.LetterBag
import Wordify.Rules.Move (Move (PlaceTiles), makeMove)
import Wordify.Rules.Player
import Wordify.Rules.Pos
import Wordify.Rules.Pos.Internal
import Wordify.Rules.ScrabbleError (ScrabbleError (CannotPlaceBlankWithoutLetter, NotAssignableToBlank))
import Wordify.Rules.Tile

letterBag :: IO LetterBag
letterBag = bagFromTiles $ map toTileBag tilesAsLetters
  where
    tilesAsLetters = "_JEARVINENVONILLEWBKONUIEUWEAZBDESIAPAEOOURGOCDSNIADOAACAR_RMYELTUTYTEREOSITNIRFGPHAQLHESOIITXFDMETG"

movePlayedWithEmptyBlankTile :: Assertion
movePlayedWithEmptyBlankTile = do
  bag <- letterBag
  game <- setupGame bag
  assertBool "Could not initialise game for test " $ isValid game

  let Right testGame = game

  let move = PlaceTiles $ placeMap "_EAR" Horizontal (8, 8)

  let moveResult = makeMove testGame move
  assertBool "Expected move not to be successful. " $ (not . isValid) moveResult
  let Left err = moveResult

  assertEqual "Unexpected result for placing tiles which do not intersect the star on the first move" (CannotPlaceBlankWithoutLetter (Pos 8 8 "H8")) err

movePlayedWithInvalidBlankTileAssignment :: Assertion
movePlayedWithInvalidBlankTileAssignment = do
  bag <- letterBag
  game <- setupGame bag
  assertBool "Could not initialise game for test " $ isValid game

  let Right testGame = game

  let move = PlaceTiles $ M.fromList [((Pos 8 8 "H8"), Blank (Just "😎"))]

  let moveResult = makeMove testGame move
  assertBool "Expected move not to be successful. " $ (not . isValid) moveResult
  let Left err = moveResult

  let validTiles = map (: []) ['A' .. 'Z']
  assertEqual "Unexpected result for placing tiles which do not intersect the star on the first move" (NotAssignableToBlank (Pos 8 8 "H8") "😎" validTiles) err

movePlayedWithValidBlankTileAssignment :: Assertion
movePlayedWithValidBlankTileAssignment = do
  bag <- letterBag
  game <- setupGame bag
  assertBool "Could not initialise game for test " $ isValid game

  let Right testGame = game

  let move = PlaceTiles $ M.fromList [(Pos 8 8 "H8", Blank (Just "H")), (Pos 8 9 "H9", Letter "I" 1)]

  let moveResult = makeMove testGame move
  assertBool "Expected move to be successful. " $ isValid moveResult
