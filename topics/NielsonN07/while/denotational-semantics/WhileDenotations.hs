module WhileDenotations where

import Prelude hiding (Num, True, False)
import WhileSyntax

data Denotations a b s = Denotations {

  -- Arithmetic expressions
    num :: Num -> a
  , var :: Var -> a
  , add :: a -> a -> a
  , mul :: a -> a -> a
  , sub :: a -> a -> a
  
  -- Boolean expressions
  , true  :: b
  , false :: b
  , eq    :: a -> a -> b
  , leq   :: a -> a -> b
  , not   :: b -> b
  , and   :: b -> b -> b

  -- Statements
  , assign :: Var -> a -> s
  , skip   :: s
  , seq    :: s -> s -> s
  , ifElse :: b -> s -> s -> s
  , while  :: b -> s -> s

}
