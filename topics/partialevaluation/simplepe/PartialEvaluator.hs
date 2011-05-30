import Syntax
import Substitution
import Evaluator (eval)
import Data.Maybe
import Control.Monad.State

type VEnv = [(String,Int)]

peval :: Prog -> (Expr, FEnv)
peval (fe,m) = runState (peval' m []) []
 where
  peval' :: Expr -> VEnv -> State FEnv Expr 
  peval' (Const i) ve = return (Const i)
  peval' (Var x) ve
   = return (case lookup x ve of
              Just v -> Const v
              Nothing -> Var x)
  peval' (Binary op e1 e2) ve
   = do
        e1' <- peval' e1 ve
        e2' <- peval' e2 ve
        return (case (e1', e2') of 
                 (Const v1, Const v2) -> Const (op2f op v1 v2)
                 _ -> Binary op e1' e2')
  peval' (IfZero e1 e2 e3) ve
   = do
        e1' <- peval' e1 ve
        case e1' of 
          (Const v1) -> if (v1==0) then r2 else r3
          _ -> r2 >>= \e2' -> r3 >>= \e3' -> return (IfZero e1' e2' e3')
   where
    r2 = peval' e2 ve
    r3 = peval' e3 ve
  peval' (Apply n es) ve
   = do
        -- Look up function
        let (ns,e) = fromJust (lookup n fe)

        -- Evaluate arguments
        es' <- mapM (flip peval' ve) es

        -- Partition arguments into static and dynamic ones
        let (ss,ds) = partition 0 ns es'

        -- Build a variable environment for the static variables
        let ve' = map (\(_,n,i) -> (n,i)) ss

        case (null ss, null ds) of

          -- A constant application
          (True, True) -> peval' e []

          -- A fully static application
          (False, True) -> peval' e ve'

          -- A fully dynamic application
          (True, False) -> return (Apply n es')

          -- A mixed static application
          (False, False) -> (do

            -- The name that encodes the static values
            let n' = n ++ "_" ++ show (map (\(p,_,i) -> (p,i)) ss)

            -- Construct application of specialized function
            let e' = Apply n' (map (\(_,_,e) -> e) ds)
        
            -- See whether the specialization exists already           
            fe <- get
            case lookup n' fe of
              Just _ -> return e'
              Nothing -> (do 

                -- Memo before possible recursion
                put (fe++[(n',undefined)])

                -- Partial evaluation of instance of function definition
                e'' <- peval' e ve'

                -- Record proper definition of specialized function
                fe' <- get
                put (update (const (map (\(_,n,_) -> n) ds,e'')) n' fe')

                -- Return application of specialized function
                return e'))
   where
    partition p [] [] = ([],[])
    partition p (n:ns) (Const i:es) = ((p,n,i):ss,ds) where (ss,ds) = partition (p+1) ns es
    partition p (n:ns) (e:es) = (ss,(p,n,e):ds) where (ss,ds) = partition (p+1) ns es


-- Update a list that is supposed to be a map/dictionary

update :: Eq k => (v -> v) -> k -> [(k,v)] -> [(k,v)]
update f k ((k',v):kvs) = if k==k' then (k',f v):kvs else (k',v):update f k kvs


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
  inlineF (s,(ss',e)) = (s,(ss,inlineE e))
  inlineFL :: [FDef] -> [FDef]
  inlineFL = map inlineF

-- Test peval on a specific value for a dynamic variable

test :: FEnv -> Expr -> String -> Int -> IO ()
test fe e s i
 = do
      let (e',fe') = peval (fe,e)
      print e'
      print fe'
      let (e'',fe'') = simplify e' fe'
      print e''
      print fe''
      let e''' = substitute [(s,Const i)] e''
      print $ eval (fe++fe'',e''') 


main
 = do
      print $ peval (lib, Apply "fac" [Const 5])
      print $ peval (lib, Apply "exp" [Const 2, Const 3])
      test lib (Apply "exp" [Var "x", Const 3]) "x" 2
      print $ peval (lib,  Apply "mod" [Const 8, Const 3, Const 0])
      test lib (Apply "mod" [Var "x", Const 3, Const 0]) "x" 8
