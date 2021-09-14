{- |
Copyright : (c) Runtime Verification 2021
License   : BSD-3-Clause
-}
module Kore.Log.ErrorEquationsSameMatch (
    ErrorEquationsSameMatch (..),
    errorEquationsSameMatch,
) where

import Control.Monad.Catch (
    Exception (..),
 )
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Kore.Equation.Equation (
    Equation,
 )
import Kore.Internal.TermLike (
    VariableName,
 )
import Log (
    Entry (..),
    MonadLog,
    Severity (Error),
    SomeEntry (SomeEntry),
    logError,
 )
import Prelude.Kore
import Pretty (
    Pretty,
    indent,
    layoutOneLine,
    pretty,
    renderText,
    vsep,
 )
import SQL (
    Table,
 )

-- | Error when RHS of equation is not a function pattern.
data ErrorEquationsSameMatch = ErrorEquationsSameMatch
    { equation1, equation2 :: Equation VariableName
    }
    deriving stock (Show, GHC.Generic)

instance SOP.Generic ErrorEquationsSameMatch

instance SOP.HasDatatypeInfo ErrorEquationsSameMatch

instance Pretty ErrorEquationsSameMatch where
    pretty ErrorEquationsSameMatch{equation1, equation2} =
        vsep
            [ "Equations"
            , indent 4 $ pretty equation1
            , "and"
            , indent 4 $ pretty equation2
            , "match the same term."
            ]

instance Exception ErrorEquationsSameMatch where
    toException = toException . SomeEntry
    fromException exn =
        fromException exn >>= fromEntry

instance Entry ErrorEquationsSameMatch where
    entrySeverity _ = Error
    helpDoc _ = "errors raised when two equations from a\
        \ function definition can match the same term"

instance SQL.Table ErrorEquationsSameMatch

-- | Error when RHS of equation is not a function pattern.
errorEquationsSameMatch ::
    MonadLog m =>
    Equation VariableName ->
    Equation VariableName ->
    m ()
errorEquationsSameMatch eq1 eq2 =
    logError . renderText . layoutOneLine . pretty 
    $ ErrorEquationsSameMatch eq1 eq2
