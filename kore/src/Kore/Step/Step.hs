{-|
Module      : Kore.Step.Step
Description : Single and multiple step execution
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Step
    ( -- * Primitive strategies
      Prim (..)
    , rewrite
    , simplify
    , rewriteStep
    , transitionRule
    , allRewrites
    , anyRewrite
    , heatingCooling
      -- * Re-exports
    , RulePattern
    , Natural
    , Strategy
    , pickLongest
    , pickFinal
    , runStrategy
    ) where

import           Control.Monad.Except
                 ( runExceptT )
import qualified Data.Foldable as Foldable
import qualified Data.Text.Prettyprint.Doc as Pretty
import           GHC.Stack
                 ( HasCallStack )
import           Numeric.Natural
                 ( Natural )

import           Kore.AST.Common
                 ( Variable )
import           Kore.AST.MetaOrObject
                 ( MetaOrObject )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import qualified Kore.Logger as Log
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.AxiomPatterns
                 ( RewriteRule (RewriteRule), RulePattern, isCoolingRule,
                 isHeatingRule, isNormalRule )
import           Kore.Step.BaseStep
                 ( OrStepResult (..), StepProof (..), applyRewriteRule )
import           Kore.Step.Representation.ExpandedPattern
                 ( CommonExpandedPattern )
import qualified Kore.Step.Representation.MultiOr as MultiOr
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier, Simplifier,
                 StepPatternSimplifier )
import qualified Kore.Step.Simplification.ExpandedPattern as ExpandedPattern
                 ( simplify )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import           Kore.Step.Strategy
import qualified Kore.Step.Strategy as Strategy
import           Kore.Unparser

{- | A strategy primitive: a rewrite rule or builtin simplification step.
 -}
data Prim rewrite = Simplify | Rewrite !rewrite

-- | Apply the rewrite.
rewrite :: rewrite -> Prim rewrite
rewrite = Rewrite

-- | Apply builtin simplification rewrites and evaluate functions.
simplify :: Prim rewrite
simplify = Simplify

{- | A single-step strategy which applies the given rewrite rule.

If the rewrite is successful, the built-in simplification rules and function
evaluator are applied (see 'ExpandedPattern.simplify' for details).

 -}
rewriteStep :: rewrite -> Strategy (Prim rewrite)
rewriteStep a =
    Strategy.sequence [Strategy.apply (rewrite a), Strategy.apply simplify]

{- | Transition rule for primitive strategies in 'Prim'.

@transitionRule@ is intended to be partially applied and passed to
'Strategy.runStrategy'.
 -}
transitionRule
    :: (HasCallStack, MetaOrObject level)
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions in patterns
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from symbol IDs to defined functions
    -> Prim (RewriteRule level Variable)
    -> (CommonExpandedPattern level, StepProof level Variable)
    -- ^ Configuration being rewritten and its accompanying proof
    -> Simplifier [(CommonExpandedPattern level, StepProof level Variable)]
transitionRule tools substitutionSimplifier simplifier axiomIdToSimplifier =
    \case
        Simplify -> transitionSimplify
        Rewrite a -> transitionRewrite a
  where
    transitionSimplify (config, proof) =
        do
            (configs, _) <-
                ExpandedPattern.simplify
                    tools
                    substitutionSimplifier
                    simplifier
                    axiomIdToSimplifier
                    config
            let
                prove config' = (config', proof)
                -- Filter out ⊥ patterns
                nonEmptyConfigs = MultiOr.filterOr configs
            return (prove <$> Foldable.toList nonEmptyConfigs)
    transitionRewrite rule (config, proof) = do
        result <-
            runExceptT
            $ applyRewriteRule
                tools
                substitutionSimplifier
                simplifier
                axiomIdToSimplifier
                config
                rule
        case result of
            Left _ ->
                (error . show . Pretty.vsep)
                    [ "Could not apply the axiom:"
                    , unparse rule
                    , "to the configuration:"
                    , unparse config
                    , "Un-implemented unification case; aborting execution."
                    ]
            Right OrStepResult { rewrittenPattern = results } ->
                Log.withLogScope "transitionRule" $
                    return $ withProof <$> Foldable.toList results
              where
                withProof result' = (result', proof)


{- | A strategy that applies all the rewrites in parallel.

After each successful rewrite, the built-in simplification rules and function
evaluator are applied (see 'ExpandedPattern.simplify' for details).

See also: 'Strategy.all'

 -}
allRewrites
    :: [rewrite]
    -> Strategy (Prim rewrite)
allRewrites rewrites =
    Strategy.all (rewriteStep <$> rewrites)

{- | A strategy that applies the rewrites until one succeeds.

The rewrites are attempted in order until one succeeds. After a successful
rewrite, the built-in simplification rules and function evaluator are applied
(see 'ExpandedPattern.simplify' for details).

See also: 'Strategy.any'

 -}
anyRewrite
    :: [rewrite]
    -> Strategy (Prim rewrite)
anyRewrite rewrites =
    Strategy.any (rewriteStep <$> rewrites)

{- | Heat the configuration, apply a normal rewrite, and cool the result.
 -}
-- TODO (thomas.tuegel): This strategy is not right because heating/cooling
-- rules must have side conditions if encoded as \rewrites, or they must be
-- \equals rules, which are not handled by this strategy.
heatingCooling
    :: (forall rewrite. [rewrite] -> Strategy (Prim rewrite))
    -- ^ 'allRewrites' or 'anyRewrite'
    -> [RewriteRule level Variable]
    -> Strategy (Prim (RewriteRule level Variable))
heatingCooling rewriteStrategy rewrites =
    Strategy.sequence [Strategy.many heat, normal, Strategy.try cool]
  where
    heatingRules = filter isHeating rewrites
    isHeating (RewriteRule rule) = isHeatingRule rule
    heat = rewriteStrategy heatingRules
    normalRules = filter isNormal rewrites
    isNormal (RewriteRule rule) = isNormalRule rule
    normal = rewriteStrategy normalRules
    coolingRules = filter isCooling rewrites
    isCooling (RewriteRule rule) = isCoolingRule rule
    cool = rewriteStrategy coolingRules