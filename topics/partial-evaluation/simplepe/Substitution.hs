module Substitution where

import Syntax


-- Substitute variables by expressions in a given expression

substitute :: [(String,Expr)] -> Expr -> Expr
substitute _ e@(Const _) = e
substitute m (Var s)
 = case (lookup s m) of
     (Just e') -> e'
     Nothing -> Var s
substitute m (Binary op e1 e2) = Binary op (substitute m e1) (substitute m e2)
substitute m (IfZero e1 e2 e3) = IfZero (substitute m e1) (substitute m e2) (substitute m e3)
substitute m (Apply s es) = Apply s (map (substitute m) es)
