{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.Simplify.Predicate (
    simplify,
    extractFirstAssignment,
) where

import Control.Error (
    MaybeT (..),
 )
import qualified Data.Functor.Foldable as Recursive
import qualified Data.Map.Strict as Map
import Data.Monoid (
    First (..),
 )
import Kore.Attribute.Pattern.FreeVariables (
    freeVariableNames,
    occursIn,
 )
import qualified Kore.Internal.Condition as Condition
import qualified Kore.Internal.Conditional as Conditional
import Kore.Internal.From
import Kore.Internal.MultiAnd (
    MultiAnd,
 )
import qualified Kore.Internal.MultiAnd as MultiAnd
import Kore.Internal.MultiOr (
    MultiOr,
 )
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.OrCondition (
    OrCondition,
 )
import qualified Kore.Internal.OrCondition as OrCondition
import Kore.Internal.OrPattern (
    OrPattern,
 )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern (
    Condition,
    Conditional (..),
    Pattern,
 )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate (
    Predicate,
    PredicateF (..),
 )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.SideCondition (
    SideCondition,
 )
import qualified Kore.Internal.SideCondition as SideCondition
import Kore.Internal.Substitution (
    pattern UnorderedAssignment,
 )
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.TermLike (TermLike)
import qualified Kore.Internal.TermLike as TermLike
import Kore.Log.WarnUnsimplifiedPredicate (
    warnUnsimplifiedPredicate,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import qualified Kore.Simplify.And as And
import qualified Kore.Simplify.AndPredicates as And
import Kore.Simplify.AndTerms (
    compareForEquals,
 )
import qualified Kore.Simplify.Ceil as Ceil
import qualified Kore.Simplify.Equals as Equals
import qualified Kore.Simplify.Iff as Iff
import qualified Kore.Simplify.Implies as Implies
import qualified Kore.Simplify.Not as Not
import qualified Kore.Simplify.Or as Or
import Kore.Simplify.Simplify
import Kore.Substitute
import Kore.Syntax (
    And (..),
    Bottom (..),
    Ceil (..),
    Equals (..),
    Exists (..),
    Floor (..),
    Forall (Forall),
    Iff (..),
    Implies (..),
    Not (..),
    Or (..),
    SomeVariableName,
    Top (..),
    variableName,
 )
import qualified Kore.Syntax.Exists as Exists
import qualified Kore.Syntax.Forall as Forall
import qualified Kore.TopBottom as TopBottom
import Kore.Unparser
import Logic
import Prelude.Kore
import qualified Pretty

{- | Simplify the 'Predicate' once.

@simplifyPredicate@ does not attempt to apply the resulting substitution and
re-simplify the result.

See also: 'simplify'
-}
simplifyPredicateTODO ::
    ( HasCallStack
    , MonadSimplify simplifier
    ) =>
    SideCondition RewritingVariableName ->
    Predicate RewritingVariableName ->
    LogicT simplifier (MultiAnd (Predicate RewritingVariableName))
simplifyPredicateTODO sideCondition predicate = do
    patternOr <-
        simplifyTermLike sideCondition (Predicate.fromPredicate_ predicate)
            & lift
    -- Despite using lift above, we do not need to
    -- explicitly check for \bottom because patternOr is an OrPattern.
    from @(Condition _) @(MultiAnd (Predicate _)) <$> scatter (OrPattern.map eraseTerm patternOr)
  where
    eraseTerm conditional
        | TopBottom.isTop (Pattern.term conditional) =
            Conditional.withoutTerm conditional
        | otherwise =
            (error . show . Pretty.vsep)
                [ "Expecting a \\top term, but found:"
                , unparse conditional
                ]

{- | @NormalForm@ is the normal form result of simplifying 'Predicate'.
 The primary purpose of this form is to transmit to the external solver.
 Note that this is almost, but not quite, disjunctive normal form; see
 'simplifyNot' for the most notable exception.
-}
type NormalForm = MultiOr (MultiAnd (Predicate RewritingVariableName))

simplify ::
    forall simplifier.
    HasCallStack =>
    MonadSimplify simplifier =>
    SideCondition RewritingVariableName ->
    Predicate RewritingVariableName ->
    simplifier NormalForm
simplify sideCondition original =
    loop 0 (mkSingleton original)
  where
    limit :: Int
    limit = 4

    loop :: Int -> NormalForm -> simplifier NormalForm
    loop count input
        | count >= limit = do
            warnUnsimplifiedPredicate limit original input
            -- Return the current NormalForm. Do not iterate further.
            pure input
        | otherwise = do
            output <- MultiAnd.traverseOrAnd worker input
            if input == output
                then pure output
                else loop (count + 1) output

    replacePredicate = SideCondition.replacePredicate sideCondition

    simplifyTerm = simplifyTermLikeOnly sideCondition

    repr = SideCondition.toRepresentation sideCondition

    worker ::
        Predicate RewritingVariableName ->
        simplifier NormalForm
    worker predicate
        | Just predicate' <- replacePredicate predicate =
            worker predicate'
        | Predicate.isSimplified repr predicate =
            pure (mkSingleton predicate)
        | otherwise =
            case predicateF of
                AndF andF -> normalizeAnd =<< traverse worker andF
                OrF orF -> normalizeOr =<< traverse worker orF
                BottomF bottomF -> normalizeBottom =<< traverse worker bottomF
                TopF topF -> normalizeTop =<< traverse worker topF
                NotF notF -> simplifyNot =<< traverse worker notF
                ImpliesF impliesF -> simplifyImplies =<< traverse worker impliesF
                IffF iffF -> simplifyIff =<< traverse worker iffF
                CeilF ceilF ->
                    simplifyCeil sideCondition =<< traverse simplifyTerm ceilF
                FloorF floorF ->
                    simplifyFloor sideCondition =<< traverse simplifyTerm floorF
                ExistsF existsF ->
                    traverse worker (Exists.refreshExists avoid existsF)
                        >>= simplifyExists sideCondition
                ForallF forallF ->
                    traverse worker (Forall.refreshForall avoid forallF)
                        >>= simplifyForall sideCondition
                EqualsF equalsF ->
                    simplifyEquals sideCondition =<< traverse simplifyTerm equalsF
                _ -> simplifyPredicateTODO sideCondition predicate & MultiOr.observeAllT
      where
        _ :< predicateF = Recursive.project predicate
        ~avoid = freeVariableNames sideCondition

-- | Construct a 'NormalForm' from a single 'Predicate'.
mkSingleton ::
    Predicate RewritingVariableName ->
    NormalForm
mkSingleton = MultiOr.singleton . MultiAnd.singleton
{-# INLINE mkSingleton #-}

-- | See 'normalizeMultiAnd'.
normalizeAnd ::
    Applicative simplifier =>
    And sort NormalForm ->
    simplifier NormalForm
normalizeAnd = normalizeMultiAnd . foldMap MultiAnd.singleton

{- | @normalizeAnd@ obeys these laws:

 Distribution:

 @
 \\and(\\or(P[1], P[2]), P[3]) = \\or(\\and(P[1], P[3]), \\and(P[2], P[3]))
 @

 Identity:

 @
 \\and(\\top, P[1]) = P[1]
 @

 Annihilation:

 @
 \\and(\\bottom, _) = \\bottom
 @

 Idempotence:

 @
 \\and(P[1], P[1]) = P[1]
 @
-}
normalizeMultiAnd ::
    Applicative simplifier =>
    MultiAnd NormalForm ->
    simplifier NormalForm
normalizeMultiAnd andOr =
    pure . MultiOr.observeAll $ do
        -- andOr: \and(\or(_, _), \or(_, _))
        andAnd <- MultiAnd.traverse Logic.scatter andOr
        -- andAnd: \and(\and(_, _), \and(_, _))
        pure (fold andAnd)

{- | If the arguments of 'Or' are already in 'NormalForm', then normalization is
 trivial.

 @normalizeOr@ obeys these laws:

 Identity:

 @
 \\or(\\bottom, P[1]) = P[1]
 @

 Annihilation:

 @
 \\or(\\top, _) = \\top
 @

 Idempotence:

 @
 \\or(P[1], P[1]) = P[1]
 @
-}
normalizeOr ::
    Applicative simplifier =>
    Or sort NormalForm ->
    simplifier NormalForm
normalizeOr = pure . fold
{-# INLINE normalizeOr #-}

-- | 'Bottom' is regarded as trivially-normalizable.
normalizeBottom ::
    Applicative simplifier =>
    Bottom sort NormalForm ->
    simplifier NormalForm
normalizeBottom _ = pure MultiOr.bottom
{-# INLINE normalizeBottom #-}

-- | 'Top' is regarded as trivially-normalizable.
normalizeTop ::
    Applicative simplifier =>
    Top sort NormalForm ->
    simplifier NormalForm
normalizeTop _ = pure (MultiOr.singleton MultiAnd.top)
{-# INLINE normalizeTop #-}

{- | @simplifyNot@ obeys these laws:

 'Top':

 @
 \\not(\\top) = \\bottom
 @

 'Bottom':

 @
 \\not(\\bottom) = \\top
 @

 'Not':

 @
 \\not(\\not(P)) = P
 @

 'Or':

 @
 \\not(\\or(P[1], P[2])) = \\and(\\not(P[1]), \\not(P[2]))
 @

 @simplifyNot@ does not expand @\not(\and(_, _))@ into @\or(_, _)@, because
 the purpose of simplification is mostly to prepare 'Predicate' for the
 external solver or for the user, and the un-expanded form is more compact.
-}
simplifyNot ::
    forall simplifier sort.
    Monad simplifier =>
    Not sort NormalForm ->
    simplifier NormalForm
simplifyNot Not{notChild = multiOr, notSort} = do
    disjunctiveNormalForms <- Logic.observeAllT $ do
        multiAnd <- Logic.scatter multiOr
        normalizeNotAnd Not{notSort, notChild = multiAnd} & lift
    normalizeMultiAnd (MultiAnd.make disjunctiveNormalForms)

normalizeNotAnd ::
    forall simplifier sort.
    Monad simplifier =>
    Not sort (MultiAnd (Predicate RewritingVariableName)) ->
    simplifier NormalForm
normalizeNotAnd Not{notSort, notChild = predicates} =
    case toList predicates of
        [] ->
            -- \not(\top)
            bottom
        [predicate] ->
            case predicateF of
                NotF Not{notChild = result} ->
                    Predicate.toMultiAnd result
                        & MultiOr.singleton
                        & pure
                _ -> fallback
          where
            _ :< predicateF = Recursive.project predicate
        _ -> fallback
  where
    fallback =
        -- \not(\and(_, ...))
        Predicate.fromMultiAnd predicates
            & fromNot
            & Predicate.markSimplified
            & mkSingleton
            & pure
    bottom = normalizeBottom Bottom{bottomSort = notSort}

{- |
 @
 \\implies(L, R) = \\or(\\not(L), \\and(L, R))
 @

 Note: @L@ is carried through to the right-hand side of 'Implies' to maximize
 the information content of that branch.
-}
simplifyImplies ::
    Monad simplifier =>
    Implies sort NormalForm ->
    simplifier NormalForm
simplifyImplies Implies{impliesFirst, impliesSecond, impliesSort} = do
    negative <- mkNotSimplified impliesFirst
    positive <- mkAndSimplified impliesFirst impliesSecond
    mkOrSimplified negative positive
  where
    mkNotSimplified notChild =
        simplifyNot Not{notSort = impliesSort, notChild}
    mkAndSimplified andFirst andSecond =
        normalizeAnd And{andSort = impliesSort, andFirst, andSecond}
    mkOrSimplified orFirst orSecond =
        normalizeOr Or{orSort = impliesSort, orFirst, orSecond}

{- |
 @
 \\iff(P[1], P[2]) = \\or(\\and(\\not(P[1]), \\not(P[2])), \\and(P[1], P[2]))
 @
-}
simplifyIff ::
    Monad simplifier =>
    Iff sort NormalForm ->
    simplifier NormalForm
simplifyIff Iff{iffFirst, iffSecond, iffSort} = do
    orFirst <- do
        andFirst <- mkNotSimplified iffFirst
        andSecond <- mkNotSimplified iffSecond
        mkAndSimplified andFirst andSecond
    orSecond <- mkAndSimplified iffFirst iffSecond
    mkOrSimplified orFirst orSecond
  where
    mkNotSimplified notChild =
        simplifyNot Not{notSort = iffSort, notChild}
    mkAndSimplified andFirst andSecond =
        normalizeAnd And{andSort = iffSort, andFirst, andSecond}
    mkOrSimplified orFirst orSecond =
        normalizeOr Or{orSort = iffSort, orFirst, orSecond}

simplifyCeil ::
    MonadSimplify simplifier =>
    SideCondition RewritingVariableName ->
    Ceil sort (OrPattern RewritingVariableName) ->
    simplifier NormalForm
simplifyCeil sideCondition =
    Ceil.simplify sideCondition >=> return . MultiOr.map (from @(Condition _))

{- |
 @
 \\floor(T) = \\not(\\ceil(\\not(T)))
 @
-}
simplifyFloor ::
    MonadSimplify simplifier =>
    SideCondition RewritingVariableName ->
    Floor sort (OrPattern RewritingVariableName) ->
    simplifier NormalForm
simplifyFloor sideCondition floor' = do
    notTerm <- mkNotSimplifiedTerm floorChild
    ceilNotTerm <- mkCeilSimplified notTerm
    mkNotSimplified ceilNotTerm
  where
    Floor{floorOperandSort, floorResultSort, floorChild} = floor'
    mkNotSimplified notChild =
        simplifyNot Not{notSort = floorResultSort, notChild}
    mkNotSimplifiedTerm notChild =
        Not.simplify sideCondition Not{notSort = floorResultSort, notChild}
    mkCeilSimplified ceilChild =
        simplifyCeil
            sideCondition
            Ceil
                { ceilOperandSort = floorOperandSort
                , ceilResultSort = floorResultSort
                , ceilChild
                }

simplifyExists ::
    forall simplifier.
    Monad simplifier =>
    SideCondition RewritingVariableName ->
    Exists () RewritingVariableName NormalForm ->
    simplifier NormalForm
simplifyExists _ = \exists@Exists{existsChild} ->
    MultiOr.traverseOr (simplifyExistsAnd . ($>) exists) existsChild
  where
    simplifyExistsAnd ::
        (Exists () RewritingVariableName)
            (MultiAnd (Predicate RewritingVariableName)) ->
        simplifier NormalForm
    simplifyExistsAnd Exists{existsVariable, existsChild}
        | not (existsVariableName `occursIn` existsChild) =
            pure (MultiOr.singleton existsChild)
        | Just value <- extractFirstAssignment existsVariableName existsChild =
            applyAssignment existsVariableName value existsChild
                & MultiOr.singleton
                & pure
        | otherwise =
            fromExists existsVariable (Predicate.fromMultiAnd existsChild)
                & mkSingleton
                & pure
      where
        existsVariableName :: SomeVariableName RewritingVariableName
        existsVariableName = inject (variableName existsVariable)

    applyAssignment ::
        SomeVariableName RewritingVariableName ->
        TermLike RewritingVariableName ->
        MultiAnd (Predicate RewritingVariableName) ->
        MultiAnd (Predicate RewritingVariableName)
    applyAssignment someVariableName termLike predicates =
        let substitution = Map.singleton someVariableName termLike
            existsChild' = MultiAnd.map (substitute substitution) predicates
            valueCeil = MultiAnd.singleton (fromCeil_ termLike)
         in existsChild' <> valueCeil

{- |
 @
 \\forall(x, P) = \\not(\\exists(x, \\not(P)))
 @
-}
simplifyForall ::
    forall simplifier.
    Monad simplifier =>
    SideCondition RewritingVariableName ->
    Forall () RewritingVariableName NormalForm ->
    simplifier NormalForm
simplifyForall sideCondition forall' = do
    notChild <- mkNotSimplified forallChild
    existsNotChild <- mkExistsSimplified notChild
    mkNotSimplified existsNotChild
  where
    Forall{forallSort, forallVariable, forallChild} = forall'
    mkNotSimplified notChild =
        simplifyNot Not{notSort = forallSort, notChild}
    mkExistsSimplified existsChild =
        simplifyExists
            sideCondition
            Exists
                { existsSort = forallSort
                , existsVariable = forallVariable
                , existsChild
                }

extractFirstAssignment ::
    SomeVariableName RewritingVariableName ->
    MultiAnd (Predicate RewritingVariableName) ->
    Maybe (TermLike RewritingVariableName)
extractFirstAssignment someVariableName predicates =
    foldMap (First . extractAssignment) predicates
        & getFirst
  where
    extractAssignment ::
        Predicate RewritingVariableName ->
        Maybe (TermLike RewritingVariableName)
    extractAssignment predicate = do
        UnorderedAssignment _ termLike <-
            Substitution.retractAssignmentFor
                someVariableName
                predicate
        guard (TermLike.isFunctionPattern termLike)
        (guard . not) (someVariableName `occursIn` termLike)
        pure termLike

{- ORMOLU_DISABLE -}
{-|'simplify' simplifies an 'Equals' pattern made of 'OrPattern's.

This uses the following simplifications
(t = term, s = substitution, p = predicate):

* Equals(a, a) = true
* Equals(phi, psi1 or psi2 or ... or psin), when phi is functional
    = or
        ( not ceil (phi) and not ceil(psi1) and ... and not ceil (psin)
        , and
            ( ceil(phi)
            , ceil(psi1) or ceil(psi2) or  ... or ceil(psin)
            , ceil(psi1) implies phi == psi1)
            , ceil(psi2) implies phi == psi2)
            ...
            , ceil(psin) implies phi == psin)
            )
        )
* Equals(t1 and t2) = ceil(t1 and t2) or (not ceil(t1) and not ceil(t2))
    if t1 and t2 are functions.
* Equals(t1 and p1 and s1, t2 and p2 and s2) =
    Or(
        And(
            Equals(t1, t2)
            And(ceil(t1) and p1 and s1, ceil(t2) and p2 and s2))
        And(not(ceil(t1) and p1 and s1), not(ceil(t2) and p2 and s2))
    )
    + If t1 and t2 can't be bottom, then this becomes
      Equals(t1 and p1 and s1, t2 and p2 and s2) =
        Or(
            And(
                Equals(t1, t2)
                And(p1 and s1, p2 and s2))
            And(not(p1 and s1), not(p2 and s2))
        )
    + If the two terms are constructors, then this becomes
      Equals(
        constr1(t1, t2, ...) and p1 and s1,
        constr2(t1', t2', ...) and p2 and s2)
        = Or(
            and(
                (p1 and s2) iff (p2 and s2),
                constr1 == constr2,
                ceil(constr1(t1, t2, ...), constr2(t1', t2', ...))
                Equals(t1, t1'), Equals(t2, t2'), ...
                )
            and(
                not(ceil(constr1(t1, t2, ...)) and p1 and s1),
                not(ceil(constr2(t1', t2', ...)), p2 and s2)
                )
        )
      Note that when expanding Equals(t1, t1') recursively we don't need to
      put the ceil conditions again, since we already asserted that.
      Also note that ceil(constr(...)) is simplifiable.
    + If the first term is a variable and the second is functional,
      then we get a substitution:
        Or(
            And(
                [t1 = t2]
                And(p1 and s1, p2 and s2))
            And(not(p1 and s1), not(p2 and s2))
        )
    + If the terms are Top, this becomes
      Equals(p1 and s1, p2 and s2) = Iff(p1 and s1, p2 and s2)
    + If the predicate and substitution are Top, then the result is any of
      Equals(t1, t2)
      Or(
          Equals(t1, t2)
          And(not(ceil(t1) and p1 and s1), not(ceil(t2) and p2 and s2))
      )

Normalization of the compared terms is not implemented yet, so
Equals(a and b, b and a) will not be evaluated to Top.
-}
{- ORMOLU_ENABLE -}
simplifyEquals ::
    MonadSimplify simplifier =>
    SideCondition RewritingVariableName ->
    Equals sort (OrPattern RewritingVariableName) ->
    simplifier NormalForm
simplifyEquals sideCondition Equals{equalsFirst = first, equalsSecond = second} =
    simplifyEvaluated sideCondition first' second'
        >>= return . MultiOr.map (from @(Condition _))
  where
    (first', second') =
        minMaxBy (on compareForEquals OrPattern.toTermLike) first second

{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (Equals Sort) (Attribute.Pattern variable) (OrPattern variable)

instead of two 'OrPattern' arguments. The type of 'makeEvaluate' may
be changed analogously. The 'Attribute.Pattern' annotation will eventually cache
information besides the pattern sort, which will make it even more useful to
carry around.

-}
simplifyEvaluated ::
    MonadSimplify simplifier =>
    SideCondition RewritingVariableName ->
    OrPattern RewritingVariableName ->
    OrPattern RewritingVariableName ->
    simplifier (OrCondition RewritingVariableName)
simplifyEvaluated sideCondition first second
    | first == second = return OrCondition.top
    -- TODO: Maybe simplify equalities with top and bottom to ceil and floor
    | otherwise = do
        let isFunctionConditional Conditional{term} = TermLike.isFunctionPattern term
        case (firstPatterns, secondPatterns) of
            ([firstP], [secondP]) ->
                makeEvaluate firstP secondP sideCondition
            ([firstP], _)
                | isFunctionConditional firstP ->
                    makeEvaluateFunctionalOr sideCondition firstP secondPatterns
            (_, [secondP])
                | isFunctionConditional secondP ->
                    makeEvaluateFunctionalOr sideCondition secondP firstPatterns
            _
                | OrPattern.isPredicate first && OrPattern.isPredicate second ->
                    Iff.simplifyEvaluated sideCondition first second
                        & fmap (MultiOr.map Pattern.withoutTerm)
                | otherwise ->
                    makeEvaluate
                        (OrPattern.toPattern first)
                        (OrPattern.toPattern second)
                        sideCondition
  where
    firstPatterns = toList first
    secondPatterns = toList second

makeEvaluateFunctionalOr ::
    forall simplifier.
    MonadSimplify simplifier =>
    SideCondition RewritingVariableName ->
    Pattern RewritingVariableName ->
    [Pattern RewritingVariableName] ->
    simplifier (OrCondition RewritingVariableName)
makeEvaluateFunctionalOr sideCondition first seconds = do
    firstCeil <-
        Ceil.makeEvaluate sideCondition first
            & fmap (OrPattern.fromOrCondition (Pattern.patternSort first))
    secondCeilsWithProofs <-
        mapM (Ceil.makeEvaluate sideCondition) seconds
            & (fmap . fmap) (OrPattern.fromOrCondition (Pattern.patternSort first))
    firstNotCeil <-
        Not.simplifyEvaluated sideCondition firstCeil
    let secondCeils = secondCeilsWithProofs
    secondNotCeils <- traverse (Not.simplifyEvaluated sideCondition) secondCeils
    let oneNotBottom = foldl' Or.simplifyEvaluated OrPattern.bottom secondCeils
    allAreBottom <-
        And.simplify
            Not.notSimplifier
            sideCondition
            (MultiAnd.make (firstNotCeil : secondNotCeils))
    firstEqualsSeconds <-
        mapM
            (makeEvaluateEqualsIfSecondNotBottom first)
            (zip seconds secondCeils)
    oneIsNotBottomEquals <-
        And.simplify
            Not.notSimplifier
            sideCondition
            (MultiAnd.make (firstCeil : oneNotBottom : firstEqualsSeconds))
    MultiOr.merge allAreBottom oneIsNotBottomEquals
        & MultiOr.map Pattern.withoutTerm
        & return
  where
    makeEvaluateEqualsIfSecondNotBottom
        Conditional{term = firstTerm}
        (Conditional{term = secondTerm}, secondCeil) =
            do
                equality <- Equals.makeEvaluateTermsAssumesNoBottom firstTerm secondTerm
                Implies.simplifyEvaluated sideCondition secondCeil equality

{- | evaluates an 'Equals' given its two 'Pattern' children.

See 'simplify' for detailed documentation.
-}
makeEvaluate ::
    MonadSimplify simplifier =>
    Pattern RewritingVariableName ->
    Pattern RewritingVariableName ->
    SideCondition RewritingVariableName ->
    simplifier (OrCondition RewritingVariableName)
makeEvaluate
    first@Conditional{term = TermLike.Top_ _}
    second@Conditional{term = TermLike.Top_ _}
    _ =
        Iff.makeEvaluate
            first{term = TermLike.mkTop_} -- remove the term's sort
            second{term = TermLike.mkTop_} -- remove the term's sort
            & MultiOr.map Pattern.withoutTerm
            & return
makeEvaluate
    Conditional
        { term = firstTerm
        , predicate = Predicate.PredicateTrue
        , substitution = (Substitution.unwrap -> [])
        }
    Conditional
        { term = secondTerm
        , predicate = Predicate.PredicateTrue
        , substitution = (Substitution.unwrap -> [])
        }
    sideCondition =
        makeEvaluateTermsToPredicate firstTerm secondTerm sideCondition
makeEvaluate
    first@Conditional{term = firstTerm}
    second@Conditional{term = secondTerm}
    sideCondition =
        do
            let first' = first{term = if termsAreEqual then TermLike.mkTop_ else firstTerm}
            firstCeil <-
                Ceil.makeEvaluate sideCondition first'
                    & fmap (OrPattern.fromOrCondition (Pattern.patternSort first'))
            let second' = second{term = if termsAreEqual then TermLike.mkTop_ else secondTerm}
            secondCeil <-
                Ceil.makeEvaluate sideCondition second'
                    & fmap (OrPattern.fromOrCondition (Pattern.patternSort second'))
            firstCeilNegation <- Not.simplifyEvaluated sideCondition firstCeil
            secondCeilNegation <- Not.simplifyEvaluated sideCondition secondCeil
            termEquality <- Equals.makeEvaluateTermsAssumesNoBottom firstTerm secondTerm
            negationAnd <-
                And.simplify
                    Not.notSimplifier
                    sideCondition
                    (MultiAnd.make [firstCeilNegation, secondCeilNegation])
            equalityAnd <-
                And.simplify
                    Not.notSimplifier
                    sideCondition
                    (MultiAnd.make [termEquality, firstCeil, secondCeil])
            Or.simplifyEvaluated equalityAnd negationAnd
                & MultiOr.map Pattern.withoutTerm
                & return
      where
        termsAreEqual = firstTerm == secondTerm

{- | Combines two terms with 'Equals' into a predicate-substitution.

It does not attempt to fully simplify the terms (the not-ceil parts used to
catch the bottom=bottom case and everything above it), but, if the patterns are
total, this should not be needed anyway.
TODO(virgil): Fully simplify the terms (right now we're not simplifying not
because it returns an 'or').

See 'simplify' for detailed documentation.
-}
makeEvaluateTermsToPredicate ::
    MonadSimplify simplifier =>
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    SideCondition RewritingVariableName ->
    simplifier (OrCondition RewritingVariableName)
makeEvaluateTermsToPredicate first second sideCondition
    | first == second = return OrCondition.top
    | otherwise = do
        result <- runMaybeT $ Equals.termEquals first second
        case result of
            Nothing ->
                return $
                    OrCondition.fromCondition . Condition.fromPredicate $
                        Predicate.markSimplified $
                            Predicate.makeEqualsPredicate first second
            Just predicatedOr -> do
                firstCeilOr <- Ceil.makeEvaluateTerm sideCondition first
                secondCeilOr <- Ceil.makeEvaluateTerm sideCondition second
                firstCeilNegation <- Not.simplifyEvaluatedPredicate firstCeilOr
                secondCeilNegation <- Not.simplifyEvaluatedPredicate secondCeilOr
                ceilNegationAnd <-
                    And.simplifyEvaluatedMultiPredicate
                        sideCondition
                        (MultiAnd.make [firstCeilNegation, secondCeilNegation])

                return $ MultiOr.merge predicatedOr ceilNegationAnd
