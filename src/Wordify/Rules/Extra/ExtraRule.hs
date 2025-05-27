module Wordify.Rules.Extra.ExtraRule (ExtraRule, makeExtraRule, applyExtraRules, RuleExecutionError(RuleExecutionError)) where
    import Control.Monad (foldM)
    import Wordify.Rules.Move (GameTransition)

    data RuleExecutionError = RuleExecutionError {errorCode :: String, description :: String }

    newtype ExtraRule = ExtraRule
        {rule :: GameTransition -> Either RuleExecutionError GameTransition}

    makeExtraRule :: (GameTransition -> Either RuleExecutionError GameTransition) -> ExtraRule
    makeExtraRule = ExtraRule

    applyExtraRules :: GameTransition -> [ExtraRule] -> Either RuleExecutionError GameTransition
    applyExtraRules = foldM applyExtraRule

    applyExtraRule :: GameTransition -> ExtraRule -> Either RuleExecutionError GameTransition
    applyExtraRule gameTransition (ExtraRule ruleFn) = ruleFn gameTransition