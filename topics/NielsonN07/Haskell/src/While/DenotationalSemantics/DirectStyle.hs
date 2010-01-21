module While.DenotationalSemantics.DirectStyle where

import qualified Prelude
import Prelude hiding (lookup, not, and)
import DenotationalSemantics.State
import While.AbstractSyntax
import While.DenotationalSemantics.Meanings
import While.DenotationalSemantics.Values


-- Types of auxiliary operators

type Cond b s
 = (s -> b) -> (s -> s) -> (s -> s) -> (s -> s)
type Fix s
 = ((s -> s) -> (s -> s)) -> (s -> s)


-- Parametric, direct-style denotational semantics

ds :: Values n b
   -> State Var n s
   -> Cond b s
   -> Fix s
   -> Meanings (s -> n) (s -> b) (s -> s)

ds v z cond fix = Meanings {

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
  , assignM = \x ma s -> update z x (ma s) s
  , skipM   = id
  , seqM    = \ms1 ms2 -> ms2 . ms1
  , ifElseM = \mb ms1 ms2 -> cond mb ms1 ms2
  , whileM  = \mb ms ->
         fix (\f -> cond mb (f . ms) id)
}
