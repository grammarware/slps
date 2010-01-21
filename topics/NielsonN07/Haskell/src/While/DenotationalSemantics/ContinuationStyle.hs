module While.DenotationalSemantics.ContinuationStyle where

import qualified Prelude
import Prelude hiding (lookup, not, and)
import DenotationalSemantics.State
import While.AbstractSyntax
import While.DenotationalSemantics.Meanings
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
    numM = \n     _ -> num v n
  , varM = \x     s -> lookup z x s
  , addM = \a1 a2 s -> add v (a1 s) (a2 s)  
  , mulM = \a1 a2 s -> mul v (a1 s) (a2 s)  
  , subM = \a1 a2 s -> sub v (a1 s) (a2 s)  

  -- Boolean expressions
  , trueM  = \      _ -> true v
  , falseM = \      _ -> false v
  , eqM    = \a1 a2 s -> eq v (a1 s) (a2 s)  
  , leqM   = \a1 a2 s -> leq v (a1 s) (a2 s)  
  , notM   = \b     s -> not v (b s) 
  , andM   = \b1 b2 s -> and v (b1 s) (b2 s)  

  -- Statements
  , assignM = \x ma c s      -> c (update z x (ma s) s)
  , skipM   =                   id
  , seqM    = \ms1 ms2       -> ms1 . ms2
  , ifElseM = \mb ms1 ms2 c  -> cond mb (ms1 c) (ms2 c)
  , whileM  = \mb ms         -> fix (\f c -> cond mb (ms (f c)) c)
}
