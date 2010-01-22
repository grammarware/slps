module While.DenotationalSemantics.ContinuationStyle where

import qualified Prelude
import Prelude hiding (id, seq, lookup, not, and)
import SemanticsLib.Main
import While.AbstractSyntax
import While.Fold


-- Continuation transformers

data CTrafoAlg t c
   = CTrafoAlg {
    id   :: t 
  , seq  :: t -> t -> t
  , cond :: c -> t -> t -> t
  , fix  :: (t -> t) -> t
}


-- Construction of continuation-style meanings

type Cont s = s -> s
type ContT s = Cont s -> Cont s

cs :: BooleanAlg b
   -> NumberAlg n b
   -> StateAlg Var n s
   -> CTrafoAlg (ContT s) (s -> b)
   -> WhileAlg (s -> n) (s -> b) (ContT s)

cs bA nA sA tA = WhileAlg {

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
  , assignF = \x ma c s   -> c $ update sA x (ma s) s
  , skipF   =                id tA
  , seqF    = \ms1 ms2    -> seq tA ms1 ms2
  , ifF     = \mb ms1 ms2 -> cond tA mb ms1 ms2
  , whileF  = \mb ms      -> fix tA (\f -> cond tA mb (seq tA ms f) (id tA))
}
