module While.DenotationalSemantics.Values where

import Prelude hiding (Num, not, and)
import qualified Prelude
import While.AbstractSyntax


-- First-order domains

data Values 
       n   -- Numbers
       b   -- Booleans
       
   = Values {

  -- Numbers
    num   :: Num -> n
  , add   :: n -> n -> n
  , mul   :: n -> n -> n
  , sub   :: n -> n -> n
  , eq    :: n -> n -> b
  , leq   :: n -> n -> b

  -- Booleans 
  , true  :: b
  , false :: b
  , not   :: b -> b
  , and   :: b -> b -> b

}


-- Standard interpretation

standardValues :: Values Integer Bool
standardValues = Values {

  -- Numbers
    num = id
  , add = (+)
  , mul = (*)
  , sub = (-)
  , eq  = (==)
  , leq = (<=)

  -- Booleans 
  , true  = Prelude.True
  , false = Prelude.False
  , not   = Prelude.not
  , and   = (&&)
}
