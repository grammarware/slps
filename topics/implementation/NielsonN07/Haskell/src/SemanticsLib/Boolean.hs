-- The domain of Booleans

module SemanticsLib.Boolean (
    BooleanAlg(BooleanAlg)
  , fromBool
  , true
  , false
  , not
  , and
  , standardBooleans
) where

import Prelude hiding (not, and)
import qualified Prelude

data BooleanAlg b 
   = BooleanAlg {
    fromBool :: Bool -> b
  , true     :: b
  , false    :: b
  , not      :: b -> b
  , and      :: b -> b -> b
}

standardBooleans :: BooleanAlg Bool
standardBooleans  = BooleanAlg {
    fromBool = id
  , true     = Prelude.True
  , false    = Prelude.False
  , not      = Prelude.not
  , and      = (&&)
}
