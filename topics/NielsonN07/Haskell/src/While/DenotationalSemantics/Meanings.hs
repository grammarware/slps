module While.DenotationalSemantics.Meanings where

import Prelude hiding (Num)
import While.AbstractSyntax

-- Composition functions for meanings
data Meanings 
      ma -- ... of arithmetic expressions
      mb -- ... of Boolean expressions
      ms -- ... of statements
   = Meanings {

    numM :: Num -> ma
  , varM :: Var -> ma
  , addM :: ma -> ma -> ma
  , mulM :: ma -> ma -> ma
  , subM :: ma -> ma -> ma
  
  , trueM  :: mb
  , falseM :: mb
  , eqM    :: ma -> ma -> mb
  , leqM   :: ma -> ma -> mb
  , notM   :: mb -> mb
  , andM   :: mb -> mb -> mb

  , assignM :: Var -> ma -> ms
  , skipM   :: ms
  , seqM    :: ms -> ms -> ms
  , ifM     :: mb -> ms -> ms -> ms
  , whileM  :: mb -> ms -> ms
}