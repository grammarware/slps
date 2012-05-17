-- Non-standard semantics of While in direct style.
-- The semantics provides a sign-detection analysis.
-- We model states as partially ordered maps from variables to numbers.
-- We do not make any reuse. (That is, we do not use fold algebras.)

module While.SignDetection.Main0 where

import Prelude hiding (True, False, lookup)
import SemanticsLib.Sign
import SemanticsLib.Map
import SemanticsLib.TT
import SemanticsLib.Domain
import SemanticsLib.Fix
import While.AbstractSyntax

-- Denotation types
type MA = PState -> Sign
type MB = PState -> TT
type MS = PState -> PState

-- Property states
type PState = Map Var Sign

-- Non-standard semantic functions
aexp :: Aexp -> MA
bexp :: Bexp -> MB
stm  :: Stm  -> MS

-- aexp :: Aexp -> MA
aexp (Num n) s = fromInteger n
aexp (Var x) s = lookup x s
aexp (Add a1 a2) s = aexp a1 s + aexp a2 s
aexp (Mul a1 a2) s = aexp a1 s * aexp a2 s
aexp (Sub a1 a2) s = aexp a1 s - aexp a2 s

-- bexp :: Bexp -> MB
bexp True  s = TT
bexp False s = FF
bexp (Eq a1 a2)  s = aexp a1 s .==. aexp a2 s
bexp (Leq a1 a2) s = aexp a1 s .<=. aexp a2 s
bexp (Not b1)    s = notTT (bexp b1 s)
bexp (And b1 b2) s = bexp b1 s `andTT` bexp b2 s

-- stm  :: Stm  -> MS
stm (Assign x a) = \s -> update x (aexp a s) s
stm Skip         = id
stm (Seq s1 s2)  = stm s2 . stm s1
stm (If b s1 s2) = cond (bexp b) (stm s1) (stm s2)
stm (While b s)  = fix (\f -> cond (bexp b) (f . stm s) id)


-- Helpers

cond :: MB -> MS -> MS -> MS
cond = \mb ms1 ms2 s ->
             case mb s of
               TT       -> ms1 s
               FF       -> ms2 s
               TopTT    -> ms1 s `lub` ms2 s
               BottomTT -> bottom

fix = fixEq2


main = 
 do
    let xpos = update "x" Pos bottom
    print xpos
    print $ stm factorial xpos

{-

> main
[("x",Pos)]
[("x",TopSign),("y",TopSign)]

-}
