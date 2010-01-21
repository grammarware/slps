module While.DenotationalSemantics.ContinuationStyle where

import qualified Prelude
import Prelude hiding (lookup, not, and)
import DenotationalSemantics.State
import While.AbstractSyntax
import While.DenotationalSemantics.Meanings (Meanings(Meanings))
import qualified While.DenotationalSemantics.Meanings as CS
import While.DenotationalSemantics.Values


-- Continuations and functions on them

type Cont s = s -> s
type ContF s = Cont s -> Cont s

-- Types of auxiliary operators

type Cond b s = (s -> b) -> Cont s -> Cont s -> Cont s
type Fix s = (ContF s -> ContF s) -> ContF s


-- Parametric, continuation-style denotational semantics

cs :: Values n b
   -> State Var n s
   -> Cond b s
   -> Fix s
   -> Meanings (s -> n) (s -> b) (ContF s)

cs v z cond fix = Meanings {

  -- Arithmetic expressions
    CS.num = \n     _ -> num v n
  , CS.var = \x     s -> lookup z x s
  , CS.add = \a1 a2 s -> add v (a1 s) (a2 s)  
  , CS.mul = \a1 a2 s -> mul v (a1 s) (a2 s)  
  , CS.sub = \a1 a2 s -> sub v (a1 s) (a2 s)  

  -- Boolean expressions
  , CS.true  = \      _ -> true v
  , CS.false = \      _ -> false v
  , CS.eq    = \a1 a2 s -> eq v (a1 s) (a2 s)  
  , CS.leq   = \a1 a2 s -> leq v (a1 s) (a2 s)  
  , CS.not   = \b     s -> not v (b s) 
  , CS.and   = \b1 b2 s -> and v (b1 s) (b2 s)  

  -- Statements
  , CS.assign = \x ma c s      -> c (update z x (ma s) s)
  , CS.skip   =                   id
  , CS.seq    = \ms1 ms2       -> ms1 . ms2
  , CS.ifElse = \mb ms1 ms2 c  -> cond mb (ms1 c) (ms2 c)
  , CS.while  = \mb ms         -> fix (\f c -> cond mb (ms (f c)) c)
}
