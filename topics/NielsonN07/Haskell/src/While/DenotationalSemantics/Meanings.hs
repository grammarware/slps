module While.DenotationalSemantics.Meanings where

import Prelude hiding (Num)
import While.AbstractSyntax

-- Composition functions for meanings
data Meanings 
       ma -- Meanings of arithmetic expressions
       mb -- Meanings of Boolean expressions
       ms -- Meanings of statements

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
  , ifElseM :: mb -> ms -> ms -> ms
  , whileM  :: mb -> ms -> ms
}