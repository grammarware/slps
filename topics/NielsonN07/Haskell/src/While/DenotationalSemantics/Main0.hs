-- Standard semantics of While in direct style.
-- We do not make any reuse.

module While.DenotationalSemantics.Main0 where

import qualified Prelude
import Prelude hiding (Num, True, False)
import While.AbstractSyntax

type State = Var -> Num

aexp :: Aexp -> State -> Num
aexp (Num n)     s = n
aexp (Var x)     s = s x
aexp (Add a1 a2) s = aexp a1 s + aexp a2 s
aexp (Mul a1 a2) s = aexp a1 s * aexp a2 s
aexp (Sub a1 a2) s = aexp a1 s - aexp a2 s

bexp :: Bexp -> State -> Bool
bexp True        s = Prelude.True
bexp False       s = Prelude.False
bexp (Eq a1 a2)  s = aexp a1 s == aexp a2 s
bexp (Leq a1 a2) s = aexp a1 s <= aexp a2 s
bexp (Not b1)    s = not (bexp b1 s)
bexp (And b1 b2) s = bexp b1 s && bexp b2 s

stm (Assign x a) = \s x' -> if x==x' then aexp a s else s x'
stm Skip         = id
stm (Seq s1 s2)  = stm s2 . stm s1
stm (If b s1 s2) = cond (bexp b) (stm s1) (stm s2)
stm (While b s)  = fix (\f -> cond (bexp b) (f . stm s) id)

cond b s1 s2 s = if b s then s1 s else s2 s
fix f = f (fix f)


main = 
 do
    let s x = if x=="x" then 5 else undefined
    print $ stm factorial s "y"

{-

> main
120

-}
