import Syntax
import Substitution
import Evaluator (eval)
import Data.Maybe

type VEnv = [(String,Expr)]

peval :: Prog -> Expr
peval (fe,m) = peval' m []
 where
  peval' :: Expr -> VEnv -> Expr
  peval' (Const i) ve = Const i
  peval' (Var x) ve
   = case lookup x ve of
      Just e -> e
      Nothing -> Var x
  peval' (Binary op e1 e2) ve
   = case (e1', e2') of 
      (Const v1, Const v2) -> Const (f v1 v2)
      _ -> Binary op e1' e2'
   where
    f  = op2f op
    e1' = peval' e1 ve
    e2' = peval' e2 ve
  peval' (IfZero e1 e2 e3) ve
   = case e1' of
      (Const v1) -> if (v1==0) then e2' else e3'
      _ -> IfZero e1' e2' e3'
   where
    e1' = peval' e1 ve
    e2' = peval' e2 ve
    e3' = peval' e3 ve
  peval' (Apply n es) ve = peval' e ve'
   where
    (ns,e) = fromJust (lookup n fe)
    es' = map (\e -> peval' e ve) es
    ve' = zip ns es'

-- Test the result of partial evaluation for a concrete value

test :: FEnv -> Expr -> String -> Int -> IO ()
test fe e s i
 = do
      let e' = peval (fe, e)
      print e'
      let e'' = substitute [(s,Const i)] e'
      print $ eval (fe, e'')

main
 = do
      print $ peval (lib, Apply "fac" [Const 5])
      print $ peval (lib, Apply "exp" [Const 2, Const 3])
      print $ peval (lib, Apply "mod" [Const 8, Const 3, Const 0])
      test lib (Apply "exp" [Var "x", Const 3]) "x" 2
--      test lib (Apply "exp" [Const 2, Var "n"]) "n" 3 -- Diverges!
--      print $ peval (lib, Apply "mod" [Var "x", Const 3]) -- Diverges!
