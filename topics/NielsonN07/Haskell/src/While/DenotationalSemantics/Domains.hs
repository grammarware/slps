module While.DenotationalSemantics.Domains where

import Prelude hiding (Num)
import While.AbstractSyntax

data Domains 
       n   -- Numbers
       b   -- Booleans
       s   -- States
       ma  -- Meanings of Arithmetic expressions
       mb  -- Meanings of Boolean expressions
       ms  -- Meanings of statements
       
   = Domains {

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

  -- States
  , lookup :: Var -> s -> n 
  , update :: Var -> n -> s -> s


  -- Meanings
  , cond :: mb -> ms -> ms -> ms
  , fix  :: (ms -> ms) -> ms
}
