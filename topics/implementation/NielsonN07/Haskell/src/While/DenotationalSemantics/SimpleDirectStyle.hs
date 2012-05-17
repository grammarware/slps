-- Parametric, direct-style denotational semantics

module While.DenotationalSemantics.SimpleDirectStyle where

import While.SimpleAbstractSyntax

type State = Var -> Z

evala :: Aexp -> State -> Z
evala (Num n) _ = n
evala (Var x) s = s x
evala (Add a1 a2) s = evala a1 s + evala a2 s
evala (Mul a1 a2) s = evala a1 s * evala a2 s
evala (Sub a1 a2) s = evala a1 s - evala a2 s

evalb :: Bexp -> State -> Bool
evalb TRUE _ = True
evalb FALSE _ = False
evalb (Eq a1 a2) s = evala a1 s == evala a2 s
evalb (Leq a1 a2) s = evala a1 s <= evala a2 s
evalb (Not b) s = not (evalb b s)
evalb (And b1 b2) s = evalb b1 s && evalb b2 s

execute :: Stm -> State -> State
execute (Assign x a) = \s -> modify s x (evala a s) 
execute Skip = id
execute (Seq s1 s2) = execute s2 . execute s1
execute (If b s1 s2) = cond (evalb b) (execute s1) (execute s2)
execute (While b s) = fix f
 where
  f g = cond (evalb b) (g . execute s) id

cond :: (State -> Bool) -> (State -> State) -> (State -> State) -> (State -> State) 
cond m0 m1 m2 s = if m0 s then m1 s else m2 s

fix f = f (fix f)

modify :: State -> Var -> Z -> State
modify s x y = \x' -> if x==x' then y else s x'
