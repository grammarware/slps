module WhileInterpreter where

import Prelude hiding (Num, True, False, not, and, seq)
import WhileSyntax
import WhileDenotations


-- Arithmetic expressions
aexp :: Denotations a b s -> Aexp -> a
aexp f (Num n)      = num f n
aexp f (Var x)      = var f x
aexp f (Add a1 a2)  = add f (aexp f a1) (aexp f a2)
aexp f (Mul a1 a2)  = mul f (aexp f a1) (aexp f a2)
aexp f (Sub a1 a2)  = sub f (aexp f a1) (aexp f a2)

-- Boolean expressions
bexp :: Denotations a b s -> Bexp -> b
bexp f True        = true f
bexp f False       = false f
bexp f (Eq a1 a2)  = eq f (aexp f a1) (aexp f a2)
bexp f (Leq a1 a2) = leq f (aexp f a1) (aexp f a2)
bexp f (Not b)     = not f (bexp f b)
bexp f (And b1 b2) = and f (bexp f b1) (bexp f b2)

-- Statements
stm :: Denotations a b s -> Stm -> s
stm f (Assign x a)     = assign f x (aexp f a)
stm f Skip             = skip f
stm f (Seq s1 s2)      = seq f (stm f s1) (stm f s2)
stm f (IfElse b s1 s2) = ifElse f (bexp f b) (stm f s1) (stm f s2)
stm f (While b s)      = while f (bexp f b) (stm f s)
