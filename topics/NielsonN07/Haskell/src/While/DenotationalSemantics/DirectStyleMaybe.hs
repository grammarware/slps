-- Parametric, direct-style denotational semantics
-- We use a Maybe type for the result of state transformers.

module While.DenotationalSemantics.DirectStyleMaybe where

import qualified Prelude
import Prelude hiding (id, seq, lookup, not, and)
import SemanticsLib.Main
import While.AbstractSyntax
import While.Fold
import While.DenotationalSemantics.DirectStyle (STrafoAlg(STrafoAlg), id, seq, cond, fix)


-- Construction of direct-style meanings

ds :: BooleanAlg b
   -> NumberAlg n b
   -> StateAlg Var n s
   -> STrafoAlg (s -> Maybe s) (s -> b)
   -> WhileAlg (s -> n) (s -> b) (s -> Maybe s)

ds bA nA sA tA = WhileAlg {

  -- Arithmetic expressions
    numF = \n     _ -> from nA n
  , varF = \x     s -> lookup sA x s
  , addF = \a1 a2 s -> add nA (a1 s) (a2 s)  
  , mulF = \a1 a2 s -> mul nA (a1 s) (a2 s)  
  , subF = \a1 a2 s -> sub nA (a1 s) (a2 s)  

  -- Boolean expressions
  , trueF  = \      _ -> true bA
  , falseF = \      _ -> false bA
  , eqF    = \a1 a2 s -> eq nA (a1 s) (a2 s)  
  , leqF   = \a1 a2 s -> leq nA (a1 s) (a2 s)  
  , notF   = \b     s -> not bA (b s) 
  , andF   = \b1 b2 s -> and bA (b1 s) (b2 s)  

  -- Statements
  , assignF = \x ma s     -> Just $ update sA x (ma s) s
  , skipF   =                id tA
  , seqF    = \ms1 ms2    -> seq tA ms1 ms2
  , ifF     = \mb ms1 ms2 -> cond tA mb ms1 ms2
  , whileF  = \mb ms      -> fix tA (\f -> cond tA mb (seq tA ms f) (id tA))
}
