module Wordify.Rules.Extra.ExtraRule (
    ExtraRule,
    makeExtraRule,
    applyExtraRule,
    applyExtraRules,
    RuleExecutionError(RuleExecutionError),
    RuleApplicationResult(RuleApplicationResult),
    RuleApplicationsResult(RuleApplicationsResult),
    finalTransition,
    bonusesApplied,
    transition,
    bonusApplied
    ) where
    
    import Control.Monad (foldM)
    import Wordify.Rules.Move (GameTransition)
    import Data.Maybe(Maybe(Just, Nothing))

    data RuleExecutionError = RuleExecutionError {errorCode :: String, description :: String }
    data BonusApplied = BonusApplied {bonusName :: String, value :: Int }
    data RuleApplicationResult = RuleApplicationResult {transition :: GameTransition, bonusApplied :: Maybe BonusApplied }
    data RuleApplicationsResult = RuleApplicationsResult {finalTransition :: GameTransition, bonusesApplied :: [BonusApplied] }

    instance Show RuleExecutionError where
        show (RuleExecutionError _ description) = description

    newtype ExtraRule = ExtraRule
        {rule :: GameTransition -> Either RuleExecutionError RuleApplicationResult}

    makeExtraRule :: (GameTransition -> Either RuleExecutionError RuleApplicationResult) -> ExtraRule
    makeExtraRule = ExtraRule

    applyExtraRules :: GameTransition -> [ExtraRule] -> Either RuleExecutionError RuleApplicationsResult
    applyExtraRules gameTransition extraRules = applyRules (RuleApplicationsResult gameTransition []) extraRules

    applyRules :: RuleApplicationsResult -> [ExtraRule] -> Either RuleExecutionError RuleApplicationsResult
    applyRules ruleApplicationResult [] = Right ruleApplicationResult
    applyRules (RuleApplicationsResult transition bonuses) (rule: rules) = do
        RuleApplicationResult newGameTransition bonusApplied <- applyExtraRule transition rule
        case bonusApplied of
            Just bonus -> applyRules (RuleApplicationsResult newGameTransition (bonus : bonuses)) rules
            Nothing -> applyRules (RuleApplicationsResult newGameTransition bonuses) rules

    applyExtraRule :: GameTransition -> ExtraRule -> Either RuleExecutionError RuleApplicationResult
    applyExtraRule gameTransition (ExtraRule ruleFn) = ruleFn gameTransition