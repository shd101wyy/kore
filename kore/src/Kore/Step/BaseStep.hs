{-|
Module      : Kore.Step.BaseStep
Description : Single step execution
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.BaseStep
    ( OrStepResult (..)
    , RulePattern
    , StepperConfiguration (..)
    , StepResult (..)
    , StepperVariable (..)
    , StepProof (..)
    , StepProofAtom (..)
    , UnificationProcedure (..)
    , VariableRenaming (..)
    , simplificationProof
    , stepProof
    , stepProofSumName
    , stepWithRemainders
    , stepWithRemaindersForUnifier
    , stepWithRule
    --
    , UnifiedRule
    , unifyRule
    , applyRule
    , applyRewriteRule
    , toConfigurationVariables
    , toAxiomVariables
    , unwrapStepperVariable
    ) where

import           Control.Applicative
                 ( Alternative (..) )
import qualified Control.Monad as Monad
import           Control.Monad.Except
import qualified Control.Monad.Morph as Monad.Morph
import qualified Control.Monad.Trans as Monad.Trans
import           Control.Monad.Trans.Except
                 ( throwE )
import qualified Data.Foldable as Foldable
import qualified Data.Hashable as Hashable
import           Data.List
                 ( foldl' )
import qualified Data.Map.Strict as Map
import           Data.Maybe
                 ( mapMaybe )
import           Data.Semigroup
                 ( Semigroup (..) )
import           Data.Sequence
                 ( Seq )
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc as Pretty
import           GHC.Generics
                 ( Generic )

import qualified Kore.Annotation.Valid as Valid
import qualified Kore.AST.Common as Common
import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import qualified Kore.Logger as Log
import           Kore.Predicate.Predicate
                 ( Predicate, makeAndPredicate, makeNotPredicate )
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.AxiomPatterns
                 ( RewriteRule (RewriteRule), RulePattern (RulePattern) )
import qualified Kore.Step.AxiomPatterns as RulePattern
import           Kore.Step.Error
import           Kore.Step.Pattern as Pattern
import           Kore.Step.Representation.ExpandedPattern
                 ( ExpandedPattern )
import qualified Kore.Step.Representation.ExpandedPattern as ExpandedPattern
import           Kore.Step.Representation.MultiAnd
                 ( MultiAnd )
import qualified Kore.Step.Representation.MultiAnd as MultiAnd
import           Kore.Step.Representation.MultiOr
                 ( MultiOr )
import qualified Kore.Step.Representation.MultiOr as MultiOr
import           Kore.Step.Representation.OrOfExpandedPattern
                 ( OrOfExpandedPattern, OrOfPredicateSubstitution )
import           Kore.Step.Representation.Predicated
                 ( Predicated (Predicated) )
import qualified Kore.Step.Representation.Predicated as Predicated
import           Kore.Step.Representation.PredicateSubstitution
                 ( PredicateSubstitution )
import qualified Kore.Step.Representation.PredicateSubstitution as PredicateSubstitution
import           Kore.Step.Simplification.Data
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import           Kore.Step.Substitution
                 ( mergePredicatesAndSubstitutionsExcept )
import qualified Kore.Step.Substitution as Substitution
import           Kore.Unification.Data
                 ( UnificationProof )
import qualified Kore.Unification.Data as Unification.Proof
import           Kore.Unification.Error
                 ( UnificationOrSubstitutionError )
import qualified Kore.Unification.Error as Unification.Error
import qualified Kore.Unification.Procedure as Unification
import           Kore.Unification.Substitution
                 ( Substitution )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unparser
import           Kore.Variables.Fresh

{-| 'StepperConfiguration' represents the configuration to which a rewriting
axiom is applied.

A configuration consists of a pattern and a condition predicate, and would be
represented as pattern /\ condition-predicate in Kore.
--}
data StepperConfiguration level = StepperConfiguration
    { stepperConfigurationPattern       :: !(CommonStepPattern level)
    -- ^ The pattern being rewritten.

    , stepperConfigurationCondition     :: !(CommonStepPattern level)
    -- ^ The condition predicate.
    -- TODO(virgil): Make this an EvaluatedCondition.
    }
    deriving (Show, Eq)

{- | 'StepProof' is the proof for an execution step or steps.
 -}
newtype StepProof (level :: *) (variable :: * -> *) =
    StepProof { getStepProof :: Seq (StepProofAtom level variable) }
  deriving (Eq, Show)

instance Hashable.Hashable (StepProof level variable) where
    hashWithSalt s _ = Hashable.hashWithSalt s (0 :: Int)

instance Semigroup (StepProof level variable) where
    (<>) (StepProof a) (StepProof b) = StepProof (a <> b)

instance Monoid (StepProof level variable) where
    mempty = StepProof mempty
    mappend = (<>)

stepProof :: StepProofAtom level variable -> StepProof level variable
stepProof atom = StepProof (Seq.singleton atom)

simplificationProof :: SimplificationProof level -> StepProof level variable
simplificationProof = stepProof . StepProofSimplification

{- | The smallest unit of a 'StepProof'.

  @StepProofAtom@ encapsulates the separate proofs resulting from unification,
  variable renaming, and simplification.

 -}
data StepProofAtom (level :: *) (variable :: * -> *)
    = StepProofUnification !(UnificationProof level variable)
    -- ^ Proof for a unification that happened during the step.
    | StepProofVariableRenamings [VariableRenaming level variable]
    -- ^ Proof for the remanings that happened during ther proof.
    | StepProofSimplification !(SimplificationProof level)
    -- ^ Proof for the simplification part of a step.
    deriving (Show, Eq, Generic)

{-| 'VariableRenaming' represents a renaming of a variable.
-}
data VariableRenaming level variable = VariableRenaming
    { variableRenamingOriginal :: variable level
    , variableRenamingRenamed  :: variable level
    }
    deriving (Show, Eq)

{- | Distinguish variables by their source (axiom or configuration).

@StepperVariable@ ensures that axiom variables are always 'LT' configuration
variables, so that the unification procedure prefers to generate substitutions
for axiom variables instead of configuration variables.

 -}
data StepperVariable variable level
    = AxiomVariable !(variable level)
    | ConfigurationVariable !(variable level)
    deriving (Show, Ord, Eq)

unwrapStepperVariable :: StepperVariable variable level -> variable level
unwrapStepperVariable (AxiomVariable variable) = variable
unwrapStepperVariable (ConfigurationVariable variable) = variable

isConfigurationVariable :: StepperVariable variable level -> Bool
isConfigurationVariable (AxiomVariable _) = False
isConfigurationVariable (ConfigurationVariable _) = True

instance
    SortedVariable variable
    => SortedVariable (StepperVariable variable)
  where
    sortedVariableSort (AxiomVariable variable) =
        sortedVariableSort variable
    sortedVariableSort (ConfigurationVariable variable) =
        sortedVariableSort variable
    fromVariable = AxiomVariable . fromVariable
    toVariable (AxiomVariable var) = toVariable var
    toVariable (ConfigurationVariable var) = toVariable var

{- | The implementation of @refreshVariable@ for 'StepperVariable' ensures that
fresh variables are always unique under projection by 'unwrapStepperVariable'.
 -}
instance
    (FreshVariable variable, SortedVariable variable) =>
    FreshVariable (StepperVariable variable)
  where
    refreshVariable (Set.map unwrapStepperVariable -> avoiding) =
        \case
            AxiomVariable variable ->
                AxiomVariable <$> refreshVariable avoiding variable
            ConfigurationVariable variable ->
                ConfigurationVariable <$> refreshVariable avoiding variable

instance
    Unparse (variable level) =>
    Unparse (StepperVariable variable level)
  where
    unparse =
        \case
            AxiomVariable var -> "Axiom" <> unparse var
            ConfigurationVariable var -> "Config" <> unparse var

{-! The result of applying an axiom to a pattern. Contains the rewritten
pattern (if any) and the unrewritten part of the original pattern.
-}
data StepResult level variable =
    StepResult
        { rewrittenPattern :: !(ExpandedPattern level variable)
        -- ^ The result of rewritting the pattern
        , remainder :: !(ExpandedPattern level variable)
        -- ^ The unrewritten part of the original pattern
        }
    deriving (Eq, Show)

{-! The result of applying an axiom to a pattern, as an Or.

Contains the rewritten pattern (if any) and the unrewritten part of the
original pattern.
-}
data OrStepResult level variable =
    OrStepResult
        { rewrittenPattern :: !(OrOfExpandedPattern level variable)
        -- ^ The result of rewritting the pattern
        , remainder :: !(OrOfExpandedPattern level variable)
        -- ^ The unrewritten part of the original pattern
        }
    deriving (Eq, Show)

{-| 'stepProofSumName' extracts the constructor name for a 'StepProof' -}
stepProofSumName :: StepProofAtom variable level -> String
stepProofSumName (StepProofUnification _)       = "StepProofUnification"
stepProofSumName (StepProofVariableRenamings _) = "StepProofVariableRenamings"
stepProofSumName (StepProofSimplification _)    = "StepProofSimplification"

-- | Wraps functions such as 'unificationProcedure' and
-- 'Kore.Step.Axiom.Matcher.matchAsUnification' to be used in
-- 'stepWithRule'.
newtype UnificationProcedure level =
    UnificationProcedure
        ( forall variable
        .   ( SortedVariable variable
            , Ord (variable level)
            , Show (variable level)
            , Unparse (variable level)
            , OrdMetaOrObject variable
            , ShowMetaOrObject variable
            , MetaOrObject level
            , FreshVariable variable
            )
        => MetadataTools level StepperAttributes
        -> PredicateSubstitutionSimplifier level
        -> StepPatternSimplifier level
        -> BuiltinAndAxiomSimplifierMap level
        -> StepPattern level variable
        -> StepPattern level variable
        -> ExceptT
            (UnificationOrSubstitutionError level variable)
            Simplifier
            ( OrOfPredicateSubstitution level variable
            , UnificationProof level variable
            )
        )

{- |
    Use the given axiom to execute a single rewriting step.

    Does not properly handle various cases, among them:
    - sigma(x, y) => y    vs    a

    Returns 'Left' only if there is an error. It is not an error if the axiom
    does not apply to the given configuration.
-}
stepWithRule
    :: forall level variable .
        ( FreshVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , OrdMetaOrObject variable
        , SortedVariable variable
        , Show (variable level)
        , ShowMetaOrObject variable
        , Unparse (variable level)
        )
    => MetadataTools level StepperAttributes
    -> UnificationProcedure level
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from symbol IDs to defined functions
    -> ExpandedPattern level variable
    -- ^ Configuration being rewritten.
    -> RulePattern level variable
    -- ^ Rewriting axiom
    -> ExceptT
        (StepError level variable)
        Simplifier
        [   ( StepResult level variable
            , StepProof level variable
            )
        ]
stepWithRule
    tools
    (UnificationProcedure unificationProcedure')
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    config
    axiom
  = Log.withLogScope "stepWithRule" $ do
    Log.logDebug
        $ "Attempting rule \n"
        <> Text.pack (show axiom)
        <> "\n for \n"
        <> Text.pack (show config)
    let configVariables = ExpandedPattern.freeVariables config
        (renaming, axiom') =
            RulePattern.refreshRulePattern configVariables axiom

        axiom'' = RulePattern.mapVariables AxiomVariable axiom'
        config' = ExpandedPattern.mapVariables ConfigurationVariable config

        RulePattern { left = axiomLeft } = axiom''
        Predicated { term = startPattern } = config'

        -- Remap unification and substitution errors into 'StepError'.
        normalizeUnificationOrSubstitutionError
            ::  ( FreshVariable variable
                , MetaOrObject level
                , Ord (variable level)
                , Show (variable level)
                )
            => ExceptT
                (UnificationOrSubstitutionError
                    level
                    (StepperVariable variable)
                )
                Simplifier
                a
            -> ExceptT (StepError level variable) Simplifier a
        normalizeUnificationOrSubstitutionError action =
            unwrapStepErrorVariables
            $ withExceptT unificationOrSubstitutionToStepError action

    -- Unify the left-hand side of the rewriting axiom with the initial
    -- configuration, producing a substitution (instantiating the axiom to the
    -- configuration) subject to a predicate.
    (unificationSolutions, unificationProof) <-
        normalizeUnificationOrSubstitutionError
            (unificationProcedure'
                tools
                substitutionSimplifier
                simplifier
                axiomIdToSimplifier
                axiomLeft
                startPattern
            )
    let unificationProof' =
            (stepProof . StepProofUnification)
                (Unification.Proof.mapVariables
                    unwrapStepperVariable
                    unificationProof
                )
        renamingProof =
            (stepProof . StepProofVariableRenamings)
                (variablePairToRenaming <$> Map.toList renaming)
          where
            variablePairToRenaming (original, renamed) =
                VariableRenaming
                    { variableRenamingOriginal = original
                    , variableRenamingRenamed  = renamed
                    }
        proof = renamingProof <> unificationProof'
        attachProof result = (result, proof)
    results <-
        traverse
            (applyUnificationToRhs
                tools
                substitutionSimplifier
                simplifier
                axiomIdToSimplifier
                axiom''
                config'
            )
            (MultiOr.extractPatterns unificationSolutions)
    return (attachProof <$> results)

applyUnificationToRhs
    :: forall level variable .
        ( Eq (variable Meta)
        , Eq (variable Object)
        , Eq (variable level)
        , FreshVariable variable
        , MetaOrObject level
        , Ord (variable Meta)
        , Ord (variable Object)
        , Ord (variable level)
        , Show (variable Meta)
        , Show (variable Object)
        , Show (variable level)
        , Unparse (variable level)
        , SortedVariable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from symbol IDs to defined functions
    -> RulePattern level (StepperVariable variable)
    -- ^ Applied rule
    -> ExpandedPattern level (StepperVariable variable)
    -- ^ Initial configuration
    -> PredicateSubstitution level (StepperVariable variable)
    -- ^ Unification solution
    -> ExceptT
        (StepError level variable)
        Simplifier
        (StepResult level variable)
applyUnificationToRhs
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    axiom@RulePattern
        { left = axiomLeft
        , right = axiomRight
        , requires = axiomRequires
        , ensures = axiomEnsures
        }
    expandedPattern@Predicated
        {term = initialTerm, substitution = initialSubstitution}
    Predicated
        { predicate = rawPredicate
        , substitution = rawSubstitution
        }
  = do
    let
        Predicated
            { predicate = startCondition
            , substitution = startSubstitution
            } = expandedPattern

    -- Combine the all the predicates and substitutions generated
    -- above and simplify the result.
    ( Predicated
            { predicate = normalizedCondition
            , substitution = normalizedSubstitution
            }
        , _proof
        ) <-
            unwrapStepErrorVariables
            $ withExceptT unificationOrSubstitutionToStepError
            $ mergePredicatesAndSubstitutionsExcept
                tools
                substitutionSimplifier
                simplifier
                axiomIdToSimplifier
                [ startCondition  -- from initial configuration
                , axiomRequires   -- from axiom
                , axiomEnsures    -- from axiom
                , rawPredicate    -- produced during unification
                ]
                [rawSubstitution, startSubstitution]

    -- Join the axiom predicate and the substitution predicate, together
    -- with the substitution in order to filter the handled values
    -- out of the initial pattern, producing the step reminder.
    ( Predicated
            { term = ()
            , predicate = normalizedRemainderPredicateRaw
            , substitution = normalizedRemainderSubstitution
            }
        , _proof
        ) <-
            unwrapStepErrorVariables
            $ withExceptT unificationOrSubstitutionToStepError
            $ mergePredicatesAndSubstitutionsExcept
                tools
                substitutionSimplifier
                simplifier
                axiomIdToSimplifier
                [ axiomRequires  -- from axiom
                , rawPredicate   -- produced during unification
                ]
                [rawSubstitution]

    let
        negatedRemainder :: Predicate level (StepperVariable variable)
        negatedRemainder =
            (makeNotPredicate . PredicateSubstitution.toPredicate)
                Predicated
                    { term = ()
                    , predicate = normalizedRemainderPredicateRaw
                    , substitution =
                        -- Note that this filtering is reasonable only because
                        -- below we check that there are no axiom variables left
                        -- in the predicate.
                        Substitution.filter isConfigurationVariable
                            normalizedRemainderSubstitution
                    }
        -- the remainder predicate is the start predicate from which we
        -- remove what was handled by the current axiom, i.e. we `and` it with
        -- the negated unification results and the axiom condition.
        normalizedRemainderPredicate
            :: Predicate level (StepperVariable variable)
        normalizedRemainderPredicate =
            makeAndPredicate
                startCondition  -- from initial configuration
                negatedRemainder

    let substitution = Substitution.toMap normalizedSubstitution

        -- Apply substitution to resulting configuration and conditions.
        rawResult = substitute substitution axiomRight

        variablesInLeftAxiom :: Set.Set (variable level)
        variablesInLeftAxiom =
            extractVariables axiomVariableFromStepper
            . Valid.freeVariables
            . extract
            $ axiomLeft
        axiomVariableFromStepper
            :: StepperVariable variable level
            -> Maybe (variable level)
        axiomVariableFromStepper (AxiomVariable v) = Just v
        axiomVariableFromStepper (ConfigurationVariable _) = Nothing
        configVariableFromStepper
            :: StepperVariable variable level
            -> Maybe (variable level)
        configVariableFromStepper (AxiomVariable _) = Nothing
        configVariableFromStepper (ConfigurationVariable v) = Just v
        extractVariables
            :: (StepperVariable variable level -> Maybe (variable level))
            -> Set.Set (StepperVariable variable level)
            -> Set.Set (variable level)
        extractVariables selector =
            Set.fromList . mapMaybe selector . Set.toList
        axiomVarsInSubstitutions :: Set.Set (variable level)
        axiomVarsInSubstitutions = extractVariables axiomVariableFromStepper
            $ Map.keysSet substitution
        configVarsInSubstitutions :: Set.Set (variable level)
        configVarsInSubstitutions = extractVariables configVariableFromStepper
            $ Map.keysSet substitution

    -- Unwrap internal 'StepperVariable's and collect the variable mappings
    -- for the proof.
    let
        result = unwrapPatternVariables rawResult
        condition = unwrapPredicateVariables normalizedCondition
        remainderPredicate = unwrapPredicateVariables normalizedRemainderPredicate

    let isBottom = Predicate.isFalse condition
        allVarsCovered = Set.isSubsetOf
                            variablesInLeftAxiom axiomVarsInSubstitutions
        symbolicPattern = not (Set.null configVarsInSubstitutions)

    when (not (isBottom || allVarsCovered || symbolicPattern))
        $ (error . unlines)
            [ "While applying axiom:", show axiom
            , "to configuration:", show expandedPattern
            , "Unexpected non-false predicate:", show condition
            , "when substitutions:", show axiomVarsInSubstitutions
            , "do not cover all variables in left axiom:"
            , show variablesInLeftAxiom
            ]

    let
        orElse :: a -> a -> a
        p1 `orElse` p2 = if isBottom then p2 else p1
    if not(isBottom) && not(allVarsCovered) && symbolicPattern
    then throwE (StepErrorUnification Unification.Error.UnsupportedPatterns)
    else return StepResult
        { rewrittenPattern = Predicated
            { term = result `orElse` mkBottom_
            , predicate = condition
            -- TODO(virgil): Can there be unused variables? Should we
            -- remove them?
            , substitution =
                (Substitution.mapVariables unwrapStepperVariable
                    $ Substitution.filter isConfigurationVariable
                    $ normalizedSubstitution
                )
                `orElse` mempty
            }
        , remainder =
            -- See docs/2019-03-06-Equality-Axiom-Configuration-Splitting.md
            -- for why this works for equality axioms.
            -- See design-decisions/2018-10-24-And-Not-Exists-Simplification.md
            -- for a similar argument for rewrite axioms, but note that it can
            -- be generalized in the same way as the equality axiom document.
            Predicated
                { term = Pattern.mapVariables unwrapStepperVariable initialTerm
                , predicate = remainderPredicate
                , substitution =
                    Substitution.mapVariables
                        unwrapStepperVariable
                        initialSubstitution
                }
        }

{-| Takes a configuration and a set of rules and tries to apply them to the
configuration in order.

The first rule is applied on the entire configuration, while the subsequent
ones are applied on the part of configuration that was not transformed by the
previous ones.

It returns all results from applying these axioms, together with the
untransformed part of the configuration left at the end (if any).

As an example, let us assume that we have the following axioms:

@
a = b if p1
a = c if p2
a = d and p3
@

and we are trying to apply them to 'a'. Then we will get the following results:

@
b and p1
c and (not p1) and p2
d and (not p1) and (not p2) and p3
a and (not p1) and (not p2) and (not p3)
@
-}
stepWithRemaindersForUnifier
    :: forall level variable .
        ( FreshVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , OrdMetaOrObject variable
        , SortedVariable variable
        , Show (variable level)
        , ShowMetaOrObject variable
        , Unparse (variable level)
        )
    => MetadataTools level StepperAttributes
    -> UnificationProcedure level
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from symbol IDs to defined functions
    -> [RulePattern level variable]
    -- ^ Rewriting axiom
    -> ExpandedPattern level variable
    -- ^ Configuration being rewritten.
    -> ExceptT
        (StepError level variable)
        Simplifier
        ( OrStepResult level variable
        , StepProof level variable
        )
stepWithRemaindersForUnifier
    _
    _
    _
    _
    _
    []
    patt
  = return
    ( OrStepResult
        { rewrittenPattern = MultiOr.make []
        , remainder = MultiOr.make [patt]
        }
    , mempty
    )
stepWithRemaindersForUnifier
    tools
    unification
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    (rule : rules)
    patt
  = do
    resultsWithProofs <-
        stepWithRule
            tools
            unification
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            patt
            rule
    let
        (results, proofs) = unzip resultsWithProofs
        rewritten :: [OrOfExpandedPattern level variable]
        remainders ::  [ExpandedPattern level variable]
        (rewritten, remainders) =
            if null results
            then ([], [patt])
            else unzip (map splitStepResult results)
    rewrittenRemaindersWithProofs <-
        mapM
            (stepWithRemaindersForUnifier
                tools
                unification
                substitutionSimplifier
                simplifier
                axiomIdToSimplifier
                rules
            )
            remainders
    let
        rewrittenRemainders :: [OrStepResult level variable]
        rewrittenRemainderProofs :: [StepProof level variable]
        (rewrittenRemainders, rewrittenRemainderProofs) =
            unzip rewrittenRemaindersWithProofs
        alreadyRewritten :: OrStepResult level variable
        alreadyRewritten =
            OrStepResult
                { rewrittenPattern =
                    MultiOr.mergeAll rewritten
                , remainder = MultiOr.make []
                }
    return
        ( foldl' mergeResults alreadyRewritten rewrittenRemainders
        , mconcat proofs <> mconcat rewrittenRemainderProofs
        )
  where
    mergeResults
        :: OrStepResult level variable
        -> OrStepResult level variable
        -> OrStepResult level variable
    mergeResults
        OrStepResult
            { rewrittenPattern = firstPattern
            , remainder = firstRemainder
            }
        OrStepResult
            { rewrittenPattern = secondPattern
            , remainder = secondRemainder
            }
      =
        OrStepResult
            { rewrittenPattern =
                MultiOr.merge firstPattern secondPattern
            , remainder =
                MultiOr.merge firstRemainder secondRemainder
            }
    splitStepResult
        :: StepResult level variable
        ->  ( OrOfExpandedPattern level variable
            , ExpandedPattern level variable
            )
    splitStepResult
        StepResult { rewrittenPattern, remainder }
      =
        ( MultiOr.make [rewrittenPattern]
        , remainder
        )

{-| Takes a configuration and a set of rules and tries to apply them to the
configuration in order, using unification.

The first rule is applied on the entire configuration, while the subsequent
ones are applied on the part of configuration that was not transformed by the
previous ones.

See 'stepWithRemaindersForUnifier' for more details.
-}
stepWithRemainders
    ::  ( FreshVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , OrdMetaOrObject variable
        , SortedVariable variable
        , Show (variable level)
        , ShowMetaOrObject variable
        , Unparse (variable level)
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from symbol IDs to defined functions
    -> ExpandedPattern level variable
    -- ^ Configuration being rewritten.
    -> [RewriteRule level variable]
    -- ^ Rewriting axiom
    -> Simplifier
        ( OrStepResult level variable
        , StepProof level variable
        )
stepWithRemainders
    tools substitutionSimplifier simplifier axiomIdToSimplifier patt rules
  = do
    resultOrError <- runExceptT
        $ stepWithRemaindersForUnifier
            tools
            (UnificationProcedure Unification.unificationProcedure)
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            (map (\ (RewriteRule rule) -> rule) rules)
            patt
    case resultOrError of
        Left _ -> error $
            "Not implemented error "
            ++ " while applying a \\rewrite axiom to the pattern "
            ++ unparseToString patt
            ++ ". We decided to end the execution because we don't understand"
            ++ " this case well enough at the moment."
        Right result -> return result

unwrapStepErrorVariables
    :: Functor m
    => ExceptT (StepError level (StepperVariable variable)) m a
    -> ExceptT (StepError level                  variable ) m a
unwrapStepErrorVariables =
    withExceptT (mapStepErrorVariables unwrapStepperVariable)

unwrapPatternVariables
    ::  forall level variable
    .   ( MetaOrObject level
        , Ord (variable level)
        , Unparse (variable level)
        )
    => StepPattern level (StepperVariable variable)
    -> StepPattern level variable
unwrapPatternVariables = Pattern.mapVariables unwrapStepperVariable

unwrapPredicateVariables
    ::  forall level variable
    .   ( MetaOrObject level
        , Ord (variable level)
        , Unparse (variable level)
        )
    => Predicate level (StepperVariable variable)
    -> Predicate level variable
unwrapPredicateVariables = fmap unwrapPatternVariables

wrapUnificationOrSubstitutionError
    :: Functor m
    => ExceptT (UnificationOrSubstitutionError level variable) m a
    -> ExceptT (StepError                      level variable) m a
wrapUnificationOrSubstitutionError =
    withExceptT unificationOrSubstitutionToStepError

{- | Lift an action from the unifier into the stepper.
 -}
liftFromUnification
    :: Monad m
    => BranchT (ExceptT (UnificationOrSubstitutionError level variable) m) a
    -> BranchT (ExceptT (StepError level variable                     ) m) a
liftFromUnification = Monad.Morph.hoist wrapUnificationOrSubstitutionError

{- | A @UnifiedRule@ has been renamed and unified with a configuration.

The rule's 'RulePattern.requires' clause is combined with the unification
solution and the renamed rule is wrapped with the combined condition.

 -}
type UnifiedRule variable =
    Predicated Object variable (RulePattern Object variable)

{- | Attempt to unify a rule with the initial configuration.

The rule variables are renamed to avoid collision with the configuration. The
rule's 'RulePattern.requires' clause is combined with the unification
solution. The combined condition is simplified and checked for
satisfiability.

If any of these steps produces an error, then @unifyRule@ returns that error.

@unifyRule@ returns the renamed rule wrapped with the combined conditions on
unification. The substitution is not applied to the renamed rule.

 -}
unifyRule
    ::  forall variable
    .   ( Ord     (variable Object)
        , Show    (variable Object)
        , Unparse (variable Object)
        , FreshVariable  variable
        , SortedVariable variable
        )
    => MetadataTools Object StepperAttributes
    -> UnificationProcedure Object
    -> PredicateSubstitutionSimplifier Object
    -> StepPatternSimplifier Object
    -> BuiltinAndAxiomSimplifierMap Object

    -> ExpandedPattern Object variable
    -- ^ Initial configuration
    -> RulePattern Object variable
    -- ^ Rule
    -> BranchT
        (ExceptT (StepError Object variable) Simplifier)
        (UnifiedRule variable)
unifyRule
    metadataTools
    (UnificationProcedure unificationProcedure)
    predicateSimplifier
    patternSimplifier
    axiomSimplifiers

    initial@Predicated { term = initialTerm }
    rule
  = do
    -- Rename free axiom variables to avoid free variables from the initial
    -- configuration.
    let
        configVariables = ExpandedPattern.freeVariables initial
        (_, rule') = RulePattern.refreshRulePattern configVariables rule
    -- Unify the left-hand side of the rule with the term of the initial
    -- configuration.
    let
        RulePattern { left = ruleLeft } = rule'
    unification <- unifyPatterns ruleLeft initialTerm
    -- Combine the unification solution with the rule's requirement clause.
    let
        RulePattern { requires = ruleRequires } = rule'
        requires' = PredicateSubstitution.fromPredicate ruleRequires
    unification' <- normalize (unification <> requires')
    return (rule' `Predicated.withCondition` unification')
  where
    unifyPatterns
        :: StepPattern Object variable
        -> StepPattern Object variable
        -> BranchT
            (ExceptT (StepError Object variable) Simplifier)
            (PredicateSubstitution Object variable)
    unifyPatterns pat1 pat2 = do
        (unifiers, _) <-
            liftFromUnification
            $ Monad.Trans.lift
            $ unificationProcedure
                metadataTools
                predicateSimplifier
                patternSimplifier
                axiomSimplifiers
                pat1
                pat2
        scatter unifiers
    normalize
        :: PredicateSubstitution Object variable
        -> BranchT
            (ExceptT (StepError Object variable) Simplifier)
            (PredicateSubstitution Object variable)
    normalize =
        liftFromUnification
        . Substitution.normalizeExcept
            metadataTools
            predicateSimplifier
            patternSimplifier
            axiomSimplifiers

{- | Apply a rule to produce final configurations given some initial conditions.

The rule should be instantiated with 'instantiateRule'. The initial conditions
are merged with any conditions from the rule instantiation and
normalized. @applyRule@ fails if normalization fails. @applyRule@ branches when
the 'PredicateSubstitutionSimplifier' causes normalization to branch.

 -}
applyRule
    ::  forall variable
    .   ( Ord     (variable Object)
        , Show    (variable Object)
        , Unparse (variable Object)
        , FreshVariable  variable
        , SortedVariable variable
        )
    => MetadataTools Object StepperAttributes
    -> PredicateSubstitutionSimplifier Object
    -> StepPatternSimplifier Object
    -> BuiltinAndAxiomSimplifierMap Object

    -> PredicateSubstitution Object variable
    -- ^ Initial conditions
    -> UnifiedRule variable
    -- ^ Non-normalized final configuration
    -> BranchT
        (ExceptT (StepError Object variable) Simplifier)
        (ExpandedPattern Object variable)
applyRule
    metadataTools
    predicateSimplifier
    patternSimplifier
    axiomSimplifiers

    initial
    unifiedRule
  = do
    -- Combine the initial conditions, the unification conditions, and the axiom
    -- ensures clause. The axiom requires clause is included by unifyRule.
    let
        Predicated { term = renamedRule } = unifiedRule
        RulePattern { ensures } = renamedRule
        ensuresCondition = PredicateSubstitution.fromPredicate ensures
        unification = Predicated.withoutTerm unifiedRule
    finalCondition <- normalize (initial <> unification <> ensuresCondition)
    -- Apply the normalized substitution to the right-hand side of the axiom.
    let
        Predicated { substitution } = finalCondition
        substitution' = Substitution.toMap substitution
        RulePattern { right = finalTerm } = renamedRule
        finalTerm' = Pattern.substitute substitution' finalTerm
    return finalCondition { ExpandedPattern.term = finalTerm' }
  where
    normalize
        :: PredicateSubstitution Object variable
        -> BranchT
            (ExceptT (StepError Object variable) Simplifier)
            (PredicateSubstitution Object variable)
    normalize =
        liftFromUnification
        . Substitution.normalizeExcept
            metadataTools
            predicateSimplifier
            patternSimplifier
            axiomSimplifiers

{- | Apply a rule to produce final configurations given some initial conditions.

The rule should be instantiated with 'instantiateRule'. The initial conditions
are merged with any conditions from the rule instantiation and
normalized. @applyRule@ fails if normalization fails. @applyRule@ branches when
the 'PredicateSubstitutionSimplifier' causes normalization to branch.

 -}
applyRemainder
    ::  forall variable
    .   ( Ord     (variable Object)
        , Show    (variable Object)
        , Unparse (variable Object)
        , FreshVariable  variable
        , SortedVariable variable
        )
    => MetadataTools Object StepperAttributes
    -> PredicateSubstitutionSimplifier Object
    -> StepPatternSimplifier Object
    -> BuiltinAndAxiomSimplifierMap Object

    -> ExpandedPattern Object variable
    -- ^ Initial configuration
    -> Predicate Object variable
    -- ^ Remainder
    -> BranchT
        (ExceptT (StepError Object variable) Simplifier)
        (ExpandedPattern Object variable)
applyRemainder
    metadataTools
    predicateSimplifier
    patternSimplifier
    axiomSimplifiers

    initial
    (PredicateSubstitution.fromPredicate -> remainder)
  = do
    let final = initial `Predicated.andCondition` remainder
        finalCondition = Predicated.withoutTerm final
        Predicated { Predicated.term = finalTerm } = final
    normalizedCondition <- normalize finalCondition
    return normalizedCondition { Predicated.term = finalTerm }
  where
    normalize
        :: PredicateSubstitution Object variable
        -> BranchT
            (ExceptT (StepError Object variable) Simplifier)
            (PredicateSubstitution Object variable)
    normalize =
        liftFromUnification
        . Substitution.normalizeExcept
            metadataTools
            predicateSimplifier
            patternSimplifier
            axiomSimplifiers

toAxiomVariables
    :: Ord (variable level)
    => RulePattern level variable
    -> RulePattern level (StepperVariable variable)
toAxiomVariables = RulePattern.mapVariables AxiomVariable

toConfigurationVariables
    :: Ord (variable level)
    => ExpandedPattern level variable
    -> ExpandedPattern level (StepperVariable variable)
toConfigurationVariables = ExpandedPattern.mapVariables ConfigurationVariable

{- | Fully apply a single rewrite rule to the initial configuration.

The rewrite rule is applied to the initial configuration to produce zero or more
final configurations.

 -}
applyRewriteRule
    ::  ( MetaOrObject level
        , Ord (variable Object)
        , Show (variable Object)
        , Unparse (variable Object)
        , FreshVariable variable
        , SortedVariable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from symbol IDs to defined functions

    -> ExpandedPattern level variable
    -- ^ Configuration being rewritten.
    -> RewriteRule level variable
    -- ^ Rewriting axiom
    -> ExceptT (StepError level variable) Simplifier
        (OrStepResult level variable)
applyRewriteRule
    metadataTools
    predicateSimplifier
    patternSimplifier
    axiomSimplifiers

    initial
    (RewriteRule rule)
  = Log.withLogScope "applyRewriteRule"
    $ do
        let
            -- Wrap the rule and configuration so that unification prefers to
            -- substitute axiom variables.
            initial' = toConfigurationVariables initial
            rule' = toAxiomVariables rule
        results <- unwrapStepErrorVariables $ gather $ do
            unifiedRule <- unifyRule' initial' rule'
            let initialCondition = Predicated.withoutTerm initial'
            final <- applyRule' initialCondition unifiedRule
            result <- checkSubstitutionCoverage initial' unifiedRule final
            let unification = Predicated.withoutTerm unifiedRule
            return (result, unification)
        let matches = snd <$> results
        remainder <- gather $ do
            remainder <- scatter (negateUnification matches)
            applyRemainder' initial remainder
        let rewrittenPattern = fst <$> results
        -- TODO (thomas.tuegel): Return the applied rules and coverage here. We
        -- probably do not want to negate the coverage to form the remainders
        -- until we have the coverage of all applied rules.
        return (OrStepResult { rewrittenPattern, remainder })
  where
    unificationProcedure = UnificationProcedure Unification.unificationProcedure
    unifyRule' =
        unifyRule
            metadataTools
            unificationProcedure
            predicateSimplifier
            patternSimplifier
            axiomSimplifiers
    applyRule' =
        applyRule
            metadataTools
            predicateSimplifier
            patternSimplifier
            axiomSimplifiers
    applyRemainder' =
        applyRemainder
            metadataTools
            predicateSimplifier
            patternSimplifier
            axiomSimplifiers

{- | Check that the final substitution covers the applied rule appropriately.

The final substitution should cover all the free variables on the left-hand side
of the applied rule; otherwise, we would wrongly introduce
universally-quantified variables into the final configuration. Failure of the
coverage check indicates a problem with unification, so in that case
@checkSubstitutionCoverage@ throws an error message with the axiom and the
initial and final configurations.

@checkSubstitutionCoverage@ calls @unwrapVariables@ to remove the axiom
variables from the substitution and unwrap all the 'StepperVariable's; this is
safe because we have already checked that all the universally-quantified axiom
variables have been instantiated by the substitution.

 -}
checkSubstitutionCoverage
    ::  ( MetaOrObject level
        , Monad m
        , SortedVariable variable
        , Ord     (variable level)
        , Show    (variable level)
        , Unparse (variable level)
        )
    => ExpandedPattern level (StepperVariable variable)
    -- ^ Initial configuration
    -> UnifiedRule (StepperVariable variable)
    -- ^ Unified rule
    -> ExpandedPattern level (StepperVariable variable)
    -- ^ Configuration after applying rule
    -> m (ExpandedPattern level variable)
checkSubstitutionCoverage initial unified final = do
    (Monad.unless checkPass . error . show . Pretty.vsep)
        [ "While applying axiom:"
        , Pretty.indent 4 (Pretty.pretty axiom)
        , "from the initial configuration:"
        , Pretty.indent 4 (unparse initial)
        , "to the final configuration:"
        , Pretty.indent 4 (unparse final)
        , "Failed substitution coverage check!"
        , "Expected substitution to cover all variables:"
        , (Pretty.indent 4 . Pretty.sep)
            (unparse <$> Set.toAscList leftAxiomVariables)
        , "in the left-hand side of the axiom."
        ]
    return (unwrapVariables final)
  where
    checkPass = isCoveringSubstitution || isInitialSymbolic
    Predicated { term = axiom } = unified
    leftAxiomVariables =
        Pattern.freeVariables leftAxiom
      where
        RulePattern { left = leftAxiom } = axiom
    Predicated { substitution } = final
    substitutionVariables = Map.keysSet (Substitution.toMap substitution)
    isCoveringSubstitution =
        Set.isSubsetOf leftAxiomVariables substitutionVariables
    isInitialSymbolic =
        (not . Set.null)
            (Set.filter isConfigurationVariable substitutionVariables)

{- | Remove axiom variables from the substitution and unwrap all variables.
 -}
unwrapVariables
    :: Ord (variable level)
    => ExpandedPattern level (StepperVariable variable)
    -> ExpandedPattern level variable
unwrapVariables config@Predicated { substitution } =
    ExpandedPattern.mapVariables unwrapStepperVariable
        config { ExpandedPattern.substitution = substitution' }
  where
    substitution' = Substitution.filter isConfigurationVariable substitution

{- | Negate the disjunction of unification solutions to form the /remainders/.

The /remainders/ are the parts of the initial configuration that are not matched
by any applied rule.

 -}
negateUnification
    ::  ( Ord     (variable Object)
        , Show    (variable Object)
        , Unparse (variable Object)
        , SortedVariable variable
        )
    => MultiOr (PredicateSubstitution Object (StepperVariable variable))
    -> MultiOr (Predicate Object variable)
negateUnification =
    fmap unwrapPredicateVariables
    . foldr negateUnification1 top
    . Foldable.toList
  where
    top = pure Predicate.makeTruePredicate
    negateUnification1 unification negations =
        Predicate.makeAndPredicate
            <$> mkNotMultiAnd conditions
            <*> negations
      where
        conditions = unificationConditions unification

mkNotMultiAnd
    ::  ( Ord     (variable Object)
        , Show    (variable Object)
        , Unparse (variable Object)
        , SortedVariable variable
        )
    => MultiAnd (Predicate Object variable)
    -> MultiOr (Predicate Object variable)
mkNotMultiAnd = MultiOr.make . map Predicate.makeNotPredicate . Foldable.toList

{- | Represent the unification solution as a conjunction of predicates.
 -}
unificationConditions
    ::  ( Ord     (variable Object)
        , Show    (variable Object)
        , Unparse (variable Object)
        , SortedVariable variable
        )
    => PredicateSubstitution Object (StepperVariable variable)
    -- ^ Unification solution
    -> MultiAnd (Predicate Object (StepperVariable variable))
unificationConditions Predicated { predicate, substitution } =
    pure predicate <|> substitutionConditions substitution'
  where
    substitution' = Substitution.filter isConfigurationVariable substitution

substitutionConditions
    ::  ( Ord     (variable Object)
        , Show    (variable Object)
        , Unparse (variable Object)
        , SortedVariable variable
        )
    => Substitution Object variable
    -> MultiAnd (Predicate Object variable)
substitutionConditions subst =
    MultiAnd.make (substitutionCoverageWorker <$> Substitution.unwrap subst)
  where
    substitutionCoverageWorker (x, t) =
        Predicate.makeEqualsPredicate (mkVar x) t