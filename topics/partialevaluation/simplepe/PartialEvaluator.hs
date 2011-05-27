import Syntax
import Data.Maybe

type VEnv = [(String,Expr)]

peval :: Prog -> Expr
peval (fe,m) = peval' m []
 where
  peval' :: Expr -> VEnv -> Expr
  peval' (Const i) ve = Const i
  peval' (Var x) ve
   = case lookup x ve of
      Just r -> r
      Nothing -> Var x
  peval' (Binary op e1 e2) ve
   = case (r1, r2) of 
      (Const v1, Const v2) -> Const (f v1 v2)
      _ -> Binary op r1 r2
   where
    f  = op2f op
    r1 = peval' e1 ve
    r2 = peval' e2 ve
  peval' (IfZero e1 e2 e3) ve
   = case r1 of
      (Const v1) -> if (v1==0) then r2 else r3
      _ -> IfZero r1 r2 r3
   where
    r1 = peval' e1 ve
    r2 = peval' e2 ve
    r3 = peval' e3 ve
  peval' (Apply n es) ve = peval' e ve'
   where
    (ns,e) = fromJust (lookup n fe)
    rs = map (\e -> peval' e ve) es
    ve' = zip ns rs

main
 = do
      print $ peval (lib, Apply "fac" [Const 5])
      print $ peval (lib, Apply "exp" [Const 2, Const 3])
      print $ peval (lib, Apply "exp" [Var "x", Const 3])
      print $ peval (lib, Apply "test" [Const 1, Const 3, Const 100000])
--      print $ peval (lib, Apply "test" [Const 1, Const 3, Var "v"])
