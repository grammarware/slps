module While.DenotationalSemantics.Meanings where

import Prelude hiding (Num)
import While.AbstractSyntax

-- Composition functions for meanings
data Meanings 
       ma -- Meanings of arithmetic expressions
       mb -- Meanings of Boolean expressions
       ms -- Meanings of statements

   = Meanings {

    num :: Num -> ma
  , var :: Var -> ma
  , add :: ma -> ma -> ma
  , mul :: ma -> ma -> ma
  , sub :: ma -> ma -> ma
  
  , true  :: mb
  , false :: mb
  , eq    :: ma -> ma -> mb
  , leq   :: ma -> ma -> mb
  , not   :: mb -> mb
  , and   :: mb -> mb -> mb

  , assign :: Var -> ma -> ms
  , skip   :: ms
  , seq    :: ms -> ms -> ms
  , ifElse :: mb -> ms -> ms -> ms
  , while  :: mb -> ms -> ms
}