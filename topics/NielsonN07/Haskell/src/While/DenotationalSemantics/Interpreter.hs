-- Interpret syntax by denotational algebra

module While.DenotationalSemantics.Interpreter where

import Prelude hiding (Num, True, False, not, and, seq)
import While.AbstractSyntax
import While.DenotationalSemantics.Meanings

interpret :: Meanings ma mb ms -> Stm -> ms
interpret m = sI
 where
  aI (Num n)      = numM m n
  aI (Var x)      = varM m x
  aI (Add a1 a2)  = addM m (aI a1) (aI a2)
  aI (Mul a1 a2)  = mulM m (aI a1) (aI a2)
  aI (Sub a1 a2)  = subM m (aI a1) (aI a2)

  bI True        = trueM m
  bI False       = falseM m
  bI (Eq a1 a2)  = eqM m (aI a1) (aI a2)
  bI (Leq a1 a2) = leqM m (aI a1) (aI a2)
  bI (Not b1)    = notM m (bI b1)
  bI (And b1 b2) = andM m (bI b1) (bI b2)

  sI (Assign x a)  = assignM m x (aI a)
  sI Skip          = skipM m
  sI (Seq s1 s2)   = seqM m (sI s1) (sI s2)
  sI (If b1 s1 s2) = ifM m (bI b1) (sI s1) (sI s2)
  sI (While b s)   = whileM m (bI b) (sI s)
