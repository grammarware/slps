module Evaluation where

import Syntax
import Prelude hiding (lookup)

lookup :: Identifier -> State -> Maybe Int
lookup e [] = Nothing
lookup e ((x,v):xvs) = if e == x then Just v else lookup e xvs

type State = [(Identifier, Int)]

evala :: AExpression -> State -> Int
evala (Number n) _ = n
evala (Identifier i) e = maybe (error "Undefined variable") id (lookup i e)
evala (Add a1 a2) e = (evala a1 e) + (evala a2 e)
evala (Sub a1 a2) e = (evala a1 e) - (evala a2 e)
evala (Mul a1 a2) e = (evala a1 e) * (evala a2 e)

evalb :: BExpression -> State -> Bool
evalb BTrue _ = True
evalb BFalse _ = False
evalb (Equals a1 a2) e = (evala a1 e) == (evala a2 e)
evalb (LessThanOrEqual a1 a2) e = (evala a1 e) <= (evala a2 e)
evalb (Not b) e = not (evalb b e)
evalb (And b1 b2) e = (evalb b1 e) && (evalb b2 e)
