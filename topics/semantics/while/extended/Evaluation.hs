module Evaluation where

import Syntax
import Data.Maybe
import Prelude hiding (lookup)

-- a frequently used pattern
match x1 x2 f1 f2 =
	if x1 == x2
		then f1
		else f2

-- see slide 546
type Environment =  [(Identifier, Location)]
type Store = [(Location, Int)] 
type Location = Int

-- see slide 553
type PEnvironment = [(Procedure, Store -> Store)]

-- see slide 547
lookup :: Environment -> Store -> Identifier -> Maybe Int
lookup env sto = store2value sto . env2loc env

env2loc :: Environment -> Identifier -> Maybe Location
env2loc [] x = Nothing
env2loc ((x1,l):xls) x2 = match x1 x2 (Just l) (env2loc xls x2)

store2value :: Store -> Maybe Location -> Maybe Int
store2value _ Nothing = Nothing
store2value [] (Just l) = Nothing
store2value ((l1,v):lvs) (Just l2) = match l1 l2 (Just v) (store2value lvs (Just l2))

-- same old thing, with State replaced by Environment and Store, as slide 546 suggested
evala :: AExpression -> Environment -> Store -> Int
evala (Number n) _ _ = n
evala (Identifier x) env sto = Data.Maybe.fromMaybe (error "Undefined variable") (lookup env sto x)
evala (Add a1 a2) env sto = evala a1 env sto + evala a2 env sto
evala (Sub a1 a2) env sto = evala a1 env sto - evala a2 env sto
evala (Mul a1 a2) env sto = evala a1 env sto * evala a2 env sto

evalb :: BExpression -> Environment -> Store -> Bool
evalb BTrue _ _ = True
evalb BFalse _ _ = False
evalb (Equals a1 a2) env sto = evala a1 env sto == evala a2 env sto
evalb (LessThanOrEqual a1 a2) env sto = evala a1 env sto <= evala a2 env sto
evalb (Not b) env sto = not (evalb b env sto)
evalb (And b1 b2) env sto = evalb b1 env sto && evalb b2 env sto
