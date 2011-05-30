module Simplifier where

import Syntax
import Substitution

-- Simplify PE result by inlining

simplify :: Expr -> FEnv -> (Expr, FEnv)
simplify e fs = simplify' e [] fs

simplify' :: Expr -> FEnv -> FEnv -> (Expr, FEnv)
simplify' e fs [] = (e,fs)
simplify' e fs1 (f@(s,(ss,e')):fs2)
 = if count == 1
     then let 
           e'' = inlineE e
           fs1' = inlineFL fs1
           fs2' = inlineFL fs2
          in simplify' e'' fs1' fs2'
     else simplify' e (fs1++[f]) fs2
 where
  -- Count the number of applications of a given function
  count = countE e + countFL fs1 + countF f + countFL fs2
  countE :: Expr -> Int
  countE (Const _) = 0
  countE (Var _) = 0
  countE (Binary _ e1 e2) = countE e1 + countE e2
  countE (IfZero e1 e2 e3) = countE e1 + countE e2 + countE e3
  countE (Apply s' es') = (if s==s' then 1 else 0) + countEL es'
  countEL :: [Expr] -> Int
  countEL = sum . map countE
  countF :: FDef -> Int
  countF (_,(_,e)) = countE e
  countFL :: [FDef] -> Int
  countFL = sum . map countF
  -- Inline the given function
  inlineE :: Expr -> Expr
  inlineE e@(Const _) = e
  inlineE e@(Var _) = e
  inlineE (Binary op e1 e2) = Binary op (inlineE e1) (inlineE e2)
  inlineE (IfZero e1 e2 e3) = IfZero (inlineE e1) (inlineE e2) (inlineE e3)
  inlineE (Apply s' es)
   = if s==s'
       then substitute (zip ss es) e' 
       else Apply s' (inlineEL es)
  inlineEL :: [Expr] -> [Expr]
  inlineEL = map inlineE
  inlineF :: FDef -> FDef
  inlineF (s,(ss',e)) = (s,(ss',inlineE e))
  inlineFL :: [FDef] -> [FDef]
  inlineFL = map inlineF
