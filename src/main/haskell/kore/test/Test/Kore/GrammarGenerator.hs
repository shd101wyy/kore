module Test.Kore.GrammarGenerator where

import           Hedgehog hiding
                 ( property )
import qualified Hedgehog.Gen as Gen
import           Test.Tasty

import qualified Data.Text as Text

import           Kore.Unparser
import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.AST.Sentence
import           Kore.Attribute.Hook
import qualified Kore.Builtin.Bool as Bool
import           Kore.IndexedModule.MetadataTools
import           Kore.Step.ExpandedPattern
import           Kore.Step.Pattern
import           Kore.Step.StepperAttributes

import Test.Kore
import Test.Kore.Builtin.Builtin
import Test.Kore.Builtin.Definition
import Test.SMT

aexpSort :: Sort Object
aexpSort = SortActualSort SortActual { sortActualName = "AExp" , sortActualSorts = [] }

bexpSort :: Sort Object
bexpSort = SortActualSort SortActual { sortActualName = "BExp" , sortActualSorts = [] }

simpleNonRecAExp :: [SentenceSymbol Object (CommonStepPattern Object)]
simpleNonRecAExp = [ mkSymbol "zero" [] [] aexpSort
                   , mkSymbol "one"  [] [] aexpSort
                   ]

simpleNonRecBExp :: [SentenceSymbol Object (CommonStepPattern Object)]
simpleNonRecBExp = [ mkSymbol "true"  [] [] bexpSort
                   , mkSymbol "false" [] [] bexpSort
                   ]

simpleRecAExp :: [SentenceSymbol Object (CommonStepPattern Object)]
simpleRecAExp = [ mkSymbol "s"    [] [aexpSort          ] aexpSort
                , mkSymbol "plus" [] [aexpSort, aexpSort] aexpSort
                ]

simpleRecBExp :: [SentenceSymbol Object (CommonStepPattern Object)]
simpleRecBExp = [ mkSymbol "and" [] [bexpSort, bexpSort] bexpSort
                , mkSymbol "or"  [] [bexpSort, bexpSort] bexpSort
                , mkSymbol "<"   [] [aexpSort, aexpSort] bexpSort
                ]

simpleRecAsPattern :: (Sort Object -> Gen (CommonStepPattern Object)) -> SentenceSymbol Object (CommonStepPattern Object) -> Gen (CommonStepPattern Object)
simpleRecAsPattern g s = applySymbol s [] <$> traverse g (sentenceSymbolSorts s)

--- syntax AExp ::= "zero"
---               | "s" AExp
---               | AExp "plus" AExp
---
--- syntax BExp ::= "true"
---               | "false"
---               | BExp "and" BExp
---               | BExp "or"  BExp
---               | AExp "<"   AExp

genVar :: Sort Object -> Gen (CommonStepPattern Object)
genVar s = mkVar <$> (Variable <$> idGen IsObject <*> pure mempty <*> pure s)

genExp :: Sort Object -> Gen (CommonStepPattern Object)
genExp s = Gen.recursive Gen.choice (genVar s : map (simpleRecAsPattern genExp) nonRecs) (map (simpleRecAsPattern genExp) recs)
    where (recs, nonRecs) | s == aexpSort = (simpleRecAExp, simpleNonRecAExp)
                          | s == bexpSort = (simpleRecBExp, simpleNonRecBExp)

unit_vacuous :: IO ()
unit_vacuous = return ()
