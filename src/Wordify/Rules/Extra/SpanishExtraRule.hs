module Wordify.Rules.ExtraRule.SpanishExtraRule where
    import Wordify.Rules.Extra.ExtraRule (ExtraRule, RuleExecutionError (RuleExecutionError), makeExtraRule)
    import qualified Data.Text as T
    import Wordify.Rules.Game (Game)
    import Wordify.Rules.Move (Move (Pass, PlaceTiles , Exchange), GameTransition (MoveTransition))
    import Wordify.Rules.FormedWord (FormedWords, FormedWord, allWords)
    import Wordify.Rules.Square (tileIfOccupied)
    import Data.Foldable (Foldable(toList))
    import Wordify.Rules.Tile (tileString, Tile)
    import Data.List (isInfixOf)
    import Control.Error (mapMaybe)
    
    spanishGameExtraRules :: [ExtraRule]
    spanishGameExtraRules = [
            makeDisallowedConsecutiveLettersRule "L" "L",
            makeDisallowedConsecutiveLettersRule "R" "R",
            makeDisallowedConsecutiveLettersRule "C" "H"
        ]

    makeDisallowedConsecutiveLettersRule :: String -> String -> ExtraRule
    makeDisallowedConsecutiveLettersRule firstTile secondTile = makeExtraRule $ \gameTransition ->
        case gameTransition of
            MoveTransition game players formedWords -> validateDoesNotResultInConsecutiveLetters gameTransition formedWords firstTile secondTile
            other -> Right other

    validateDoesNotResultInConsecutiveLetters :: GameTransition -> FormedWords -> String -> String -> Either RuleExecutionError GameTransition
    validateDoesNotResultInConsecutiveLetters gameTransition formedWords firstTile secondTile
        | any (containsConsecutiveTiles firstTile secondTile) allFormedWords = Left (RuleExecutionError "InvalidConsecutiveTiles" ruleDescription)
        | otherwise = Right gameTransition
        where
            allFormedWords = allWords formedWords
            ruleDescription = concat ["Cannot place ", firstTile, " and ", secondTile, " consecutively"]

    containsConsecutiveTiles :: String -> String -> FormedWord -> Bool
    containsConsecutiveTiles firstTile secondTile formedWord =
        let tiles = mapMaybe (tileIfOccupied . snd) (toList formedWord)
        in containsConsecutive firstTile secondTile tiles

    containsConsecutive :: String -> String -> [Tile] -> Bool
    containsConsecutive firstTile secondTile tiles = [firstTile, secondTile] `isInfixOf` tileLetters tiles
        where
            tileLetters tiles = mapMaybe tileString tiles