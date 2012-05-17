module Main where

import Data.Maybe

-- assignment 5, part 1
{-
lookup(M,X,Y) :- append(_,[(X,Y)|_],M).
update([],X,Y,[(X,Y)]).
update([(X,_)|M],X,Y,[(X,Y)|M]).
update([(X1,Y1)|M1],X2,Y2,[(X1,Y1)|M2]) :- \+ X1 = X2, update(M1,X2,Y2,M2).
-}
mylookup :: Identifier -> LookupTable -> Maybe Int
mylookup e [] = Nothing
mylookup e ((x,v):xvs) = if e == x then Just v else mylookup e xvs

myupdate :: LookupTable -> Identifier -> Int -> LookupTable
myupdate [] x v = [(x, v)]
myupdate ((x1,v1):xvs) x2 v2 =
    if x1 == x2
    then (x1,v2):xvs
    else (x1,v1):myupdate xvs x2 v2

-- assignment 5, part 2
data Statement
 = SList Statement Statement
 | Skip
 | Assign Identifier AExpression
 | IfThenElse BExpression Statement Statement
 | While BExpression Statement

data AExpression
 = Number Int
 | Identifier String
 | Add AExpression AExpression
 | Sub AExpression AExpression
 | Mul AExpression AExpression

data BExpression
 = BTrue
 | BFalse
 | Equals AExpression AExpression
 | LessThanOrEqual AExpression AExpression
 | Not BExpression
 | And BExpression BExpression

type Identifier = String
type LookupTable = [(Identifier, Int)]

{- 
y:=2;
x:=(y+4);
if (y = 2 ^ x = 6)
 then
  z := y
 else
  z := 1000;
while x≤10 do x := (x + 1)
-}
test = SList (Assign "y" (Number 2))
      (SList (Assign "x" (Add (Identifier "y") (Number 4)))
      (SList (IfThenElse (And (Equals (Identifier "y") (Number 2)) (Equals (Identifier "x") (Number 6)))
                         (Assign "z" (Identifier "y"))
                         (Assign "z" (Number 1000)))
      (While (LessThanOrEqual (Identifier "x") (Number 10))
             (Assign "x" (Add (Identifier "x") (Number 1))))))

skips = SList Skip Skip

evals :: Statement -> LookupTable -> LookupTable
evals (SList s1 s2) e = evals s2 (evals s1 e)
evals Skip e = e
evals (Assign i a) e = myupdate e i (evala a e)
evals (IfThenElse b st se) e = if evalb b e then evals st e else evals se e
evals (While b s) e = if evalb b e then evals (While b s) (evals s e) else e

evala :: AExpression -> LookupTable -> Int
evala (Number n) _ = n
evala (Identifier i) e = Data.Maybe.fromMaybe (error "Undefined variable") (mylookup i e)
evala (Add a1 a2) e = evala a1 e + evala a2 e
evala (Sub a1 a2) e = evala a1 e - evala a2 e
evala (Mul a1 a2) e = evala a1 e * evala a2 e

evalb :: BExpression -> LookupTable -> Bool
evalb BTrue _ = True
evalb BFalse _ = False
evalb (Equals a1 a2) e = evala a1 e == evala a2 e
evalb (LessThanOrEqual a1 a2) e = evala a1 e <= evala a2 e
evalb (Not b) e = not (evalb b e)
evalb (And b1 b2) e = evalb b1 e && evalb b2 e

main =
	print (evals test [])
