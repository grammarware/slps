module While.DenotationalSemantics.DirectStyleMaybe where

import qualified Prelude
import Prelude hiding (lookup, not, and)
import While.AbstractSyntax
import While.DenotationalSemantics.Meanings (Meanings(Meanings))
import qualified While.DenotationalSemantics.Meanings as DS
import While.DenotationalSemantics.Values
import While.DenotationalSemantics.State


-- Types of auxiliary operators

type Cond b s
 = (s -> b) -> (s -> Maybe s) -> (s -> Maybe s) -> (s -> Maybe s)
type Fix s
 = ((s -> Maybe s) -> (s -> Maybe s)) -> (s -> Maybe s)


-- Parametric, direct-style denotational semantics

ds :: Values n b
   -> State n s
   -> Cond b s
   -> Fix s
   -> Meanings (s -> n) (s -> b) (s -> Maybe s)

ds v z cond fix = Meanings {

  -- Arithmetic expressions
    DS.num = \n     _ -> num v n
  , DS.var = \x     s -> lookup z x s
  , DS.add = \a1 a2 s -> add v (a1 s) (a2 s)  
  , DS.mul = \a1 a2 s -> mul v (a1 s) (a2 s)  
  , DS.sub = \a1 a2 s -> sub v (a1 s) (a2 s)  

  -- Boolean expressions
  , DS.true  = \      _ -> true v
  , DS.false = \      _ -> false v
  , DS.eq    = \a1 a2 s -> eq v (a1 s) (a2 s)  
  , DS.leq   = \a1 a2 s -> leq v (a1 s) (a2 s)  
  , DS.not   = \b     s -> not v (b s) 
  , DS.and   = \b1 b2 s -> and v (b1 s) (b2 s)  

  -- Statements
  , DS.assign = \x ma s -> Just $ update z x (ma s) s
  , DS.skip   = Just
  , DS.seq    = \ms1 ms2 s -> ms1 s >>= ms2
  , DS.ifElse = \mb ms1 ms2 -> cond mb ms1 ms2
  , DS.while  = \mb ms -> fix (\f -> cond mb (\s -> ms s >>= f) Just)
}
