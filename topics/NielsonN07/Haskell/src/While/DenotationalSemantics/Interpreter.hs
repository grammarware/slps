-- Interpret syntax by denotational algebra

module While.DenotationalSemantics.Interpreter where

import Prelude hiding (Num, True, False, not, and, seq)
import While.AbstractSyntax
import While.DenotationalSemantics.Meanings


-- Interpret arithmetic expressions
aexp :: Meanings ma mb ms -> Aexp -> ma
aexp m (Num n)      = num m n
aexp m (Var x)      = var m x
aexp m (Add a1 a2)  = add m (aexp m a1) (aexp m a2)
aexp m (Mul a1 a2)  = mul m (aexp m a1) (aexp m a2)
aexp m (Sub a1 a2)  = sub m (aexp m a1) (aexp m a2)

-- Interpret Boolean expressions
bexp :: Meanings ma mb ms -> Bexp -> mb
bexp m True        = true m
bexp m False       = false m
bexp m (Eq a1 a2)  = eq m (aexp m a1) (aexp m a2)
bexp m (Leq a1 a2) = leq m (aexp m a1) (aexp m a2)
bexp m (Not b)     = not m (bexp m b)
bexp m (And b1 b2) = and m (bexp m b1) (bexp m b2)

-- Interpret statements
stm :: Meanings ma mb ms -> Stm -> ms
stm m (Assign x a)     = assign m x (aexp m a)
stm m Skip             = skip m
stm m (Seq s1 s2)      = seq m (stm m s1) (stm m s2)
stm m (IfElse b s1 s2) = ifElse m (bexp m b) (stm m s1) (stm m s2)
stm m (While b s)      = while m (bexp m b) (stm m s)
