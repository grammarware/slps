module While.ProgramAnalysis.Values where

import Prelude hiding (Num, not, and)
import qualified Prelude
import While.AbstractSyntax
import While.DenotationalSemantics.Values
import While.ProgramAnalysis.Domains
import While.ProgramAnalysis.TT (TT(TT, FF), (.==.), (.<=.))
import qualified While.ProgramAnalysis.TT as TT
import While.ProgramAnalysis.Sign


-- Abstract interpretation

abstractValues :: Values Sign TT
abstractValues = Values {

  -- Numbers
    num = fromInteger
  , add = (+)
  , mul = (*)
  , sub = (-)
  , eq  = (.==.)
  , leq = (.<=.)

  -- Booleans 
  , true  = TT
  , false = FF
  , not   = TT.not
  , and   = (TT.&&)
}
