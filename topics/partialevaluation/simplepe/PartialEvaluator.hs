import Syntax
import Data.Maybe
import Control.Monad.State

type VEnv = [(String,Int)]

peval :: Prog -> VEnv -> (Expr, FEnv)
peval (fe,m) ve = runState (peval' m ve) []
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

        -- The name that encodes the static values
        let n' = n ++ "_" ++ show (map (\(p,_,i) -> (p,i)) ss)

        -- The result
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
                         return e')
   where
    partition p [] [] = ([],[])
    partition p (n:ns) (Const i:es) = ((p,n,i):ss,ds) where (ss,ds) = partition (p+1) ns es
    partition p (n:ns) (e:es) = (ss,(p,n,e):ds) where (ss,ds) = partition (p+1) ns es


-- Update a list that is supposed to be a map/dictionary

update :: Eq k => (v -> v) -> k -> [(k,v)] -> [(k,v)]
update f k ((k',v):kvs) = if k==k' then (k',f v):kvs else (k',v):update f k kvs


-- Test peval on a specific value for a dynamic variable

test :: FEnv -> Expr -> String -> Int -> IO ()
test fe e s i
 = do
      let (e',fe') = peval (fe,e) []
      print e'
      print fe'
      print $ peval (fe++fe',e') [(s,i)]

main
 = do
      print $ peval (lib, Apply "fac" [Const 5]) []
      print $ peval (lib, Apply "exp" [Const 2, Const 3]) []
      test lib (Apply "exp" [Var "x", Const 3]) "x" 2
      print $ peval (lib, Apply "test" [Const 1, Const 3, Const 10]) []
      test lib (Apply "test" [Const 1, Const 3, Var "v"]) "v" 10
