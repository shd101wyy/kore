{- |
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
-}
module Kore.Simplify.InternalBool (
    simplify,
) where

import Kore.Internal.InternalBool
import Kore.Internal.OrPattern (
    OrPattern,
 )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.TermLike
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Prelude.Kore

simplify ::
    InternalBool ->
    OrPattern RewritingVariableName
simplify = OrPattern.fromPattern . pure . mkInternalBool