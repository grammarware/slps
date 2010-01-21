module While.SignDetection.Values where

import Prelude hiding (Num, not, and)
import qualified Prelude
import ProgramAnalysis.Domains
import ProgramAnalysis.TT (TT(TT, FF), (.==.), (.<=.))
import qualified ProgramAnalysis.TT as TT
import While.AbstractSyntax
import While.DenotationalSemantics.Values
import While.SignDetection.Sign


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
