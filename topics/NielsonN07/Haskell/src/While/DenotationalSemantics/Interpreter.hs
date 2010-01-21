-- Interpret syntax by denotational algebra

module While.DenotationalSemantics.Interpreter where

import Prelude hiding (Num, True, False, not, and, seq)
import While.AbstractSyntax
import While.DenotationalSemantics.Meanings


-- Interpret arithmetic expressions
aexp :: Meanings ma mb ms -> Aexp -> ma
aexp m (Num n)      = numM m n
aexp m (Var x)      = varM m x
aexp m (Add a1 a2)  = addM m (aexp m a1) (aexp m a2)
aexp m (Mul a1 a2)  = mulM m (aexp m a1) (aexp m a2)
aexp m (Sub a1 a2)  = subM m (aexp m a1) (aexp m a2)

-- Interpret Boolean expressions
bexp :: Meanings ma mb ms -> Bexp -> mb
bexp m True        = trueM m
bexp m False       = falseM m
bexp m (Eq a1 a2)  = eqM m (aexp m a1) (aexp m a2)
bexp m (Leq a1 a2) = leqM m (aexp m a1) (aexp m a2)
bexp m (Not b)     = notM m (bexp m b)
bexp m (And b1 b2) = andM m (bexp m b1) (bexp m b2)

-- Interpret statements
stm :: Meanings ma mb ms -> Stm -> ms
stm m (Assign x a) = assignM m x (aexp m a)
stm m Skip         = skipM m
stm m (Seq s1 s2)  = seqM m (stm m s1) (stm m s2)
stm m (IfElse b s1 s2)
 = ifElseM m (bexp m b) (stm m s1) (stm m s2)
stm m (While b s)
 = whileM m (bexp m b) (stm m s)
