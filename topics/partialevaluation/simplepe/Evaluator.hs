module Evaluator where

import Syntax
import Data.Maybe

type VEnv = [(String,Int)]

eval :: Prog -> Int
eval (fe,m) = eval' m []
 where
  eval' :: Expr -> VEnv -> Int
  eval' (Const i) ve = i
  eval' (Var x) ve = fromJust (lookup x ve)
  eval' (Binary op e1 e2) ve = f v1 v2
   where
    f  = op2f op
    v1 = eval' e1 ve
    v2 = eval' e2 ve
  eval' (IfZero e1 e2 e3) ve = if (v1==0) then v2 else v3
   where
    v1 = eval' e1 ve
    v2 = eval' e2 ve
    v3 = eval' e3 ve
  eval' (Apply n es) ve = eval' e ve'
   where
    (ns,e) = fromJust (lookup n fe)
    vs = map (\e -> eval' e ve) es
    ve' = zip ns vs

main
 = do
      print $ eval (lib, Apply "fac" [Const 5])
      print $ eval (lib, Apply "exp" [Const 2, Const 3])
      print $ eval (lib, Apply "mod" [Const 8, Const 3])
