module WhileDirectStyle where

import qualified Prelude
import Prelude hiding (lookup, id, not, and)
import WhileSyntax
import WhileDenotations (Denotations(Denotations))
import qualified WhileDenotations as DS
import WhileDomains


-- Parametric, direct-style denotational semantics

ds :: Domains n b s (s -> n) (s -> b) (s -> s')
   -> Denotations   (s -> n) (s -> b) (s -> s')

ds d = Denotations {

  -- Arithmetic expressions
    DS.num = \n     _ -> num d n
  , DS.var = \x     s -> lookup d x s
  , DS.add = \a1 a2 s -> add d (a1 s) (a2 s)  
  , DS.mul = \a1 a2 s -> mul d (a1 s) (a2 s)  
  , DS.sub = \a1 a2 s -> sub d (a1 s) (a2 s)  

  -- Boolean expressions
  , DS.true  = \      _ -> true d
  , DS.false = \      _ -> false d
  , DS.eq    = \a1 a2 s -> eq d (a1 s) (a2 s)  
  , DS.leq   = \a1 a2 s -> leq d (a1 s) (a2 s)  
  , DS.not   = \b     s -> not d (b s) 
  , DS.and   = \b1 b2 s -> and d (b1 s) (b2 s)  

  -- Statements
  , DS.assign = \x a     -> mk d (\s -> update d x (a s) s)
  , DS.skip   =             id d
  , DS.seq    = \t1 t2   -> o d t2 t1
  , DS.ifElse = \b t1 t2 -> cond d b t1 t2
  , DS.while  = \b t     -> fix d (\f -> cond d b (o d f t) (id d))

}
