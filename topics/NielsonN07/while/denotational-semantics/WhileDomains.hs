module WhileDomains where

import Prelude hiding (Num)
import WhileSyntax

data Domains 
       n  -- Numbers
       b  -- Booleans
       s  -- States
       a  -- Meanings of arithmetic expressions
       l  -- Meanings of Boolean (logical) expressions
       t  -- Meanings of statements (i.e., statement transformers)
       
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

  -- State transformers
  , mk   :: (s -> s) -> t
  , id   :: t
  , o    :: t -> t -> t
  , cond :: l -> t -> t -> t
  , fix  :: (t -> t) -> t

}
