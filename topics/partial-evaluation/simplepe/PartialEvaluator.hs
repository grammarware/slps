import Syntax
import Substitution
import Simplifier
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

     -- Partially evaluate arguments
     es' <- mapM (flip peval' ve) es

     -- Partition arguments into static and dynamic ones
     let (ss,ds) = partition ns es'

     case (null ss, null ds) of

       -- A constant application
       (True, True) -> peval' e []

       -- A fully static application
       (False, True) -> peval' e ss

       -- A fully dynamic application
       (True, False) -> return (Apply n es')

       -- A mixed static application
       (False, False) -> (do

         -- The name that encodes the static values
         let n' = n ++ "_" ++ myshowl ss

         -- Construct application of specialized function
         let e' = Apply n' (map snd ds)
        
         -- See whether the specialization exists already           
         fe <- get
         case lookup n' fe of
           Just _ -> return e'
           Nothing -> (do 

             -- Memo before possible recursion
             put (fe++[(n',undefined)])

             -- Partial evaluation of function body
             e'' <- peval' e ss

             -- Record definition of specialized function
             fe' <- get
             put (update (const (map fst ds,e'')) n' fe')

             -- Return application of specialized function
             return e'))
   where
    partition [] [] = ([],[])
    partition (n:ns) (Const i:es) = ((n,i):ss,ds) where (ss,ds) = partition ns es
    partition (n:ns) (e:es) = (ss,(n,e):ds) where (ss,ds) = partition ns es
    myshowl [] = "[]"
    myshowl (x:xs) = "[" ++ myshow1 x ++ myshowr xs ++ "]"
    myshow1 (s,i) = "(" ++ s ++ "," ++ show i ++ ")"
    myshowr [] = ""
    myshowr (x:xs) = "," ++ myshow1 x ++ myshowr xs

-- Update a list that is supposed to be a map/dictionary

update :: Eq k => (v -> v) -> k -> [(k,v)] -> [(k,v)]
update f k ((k',v):kvs) = if k==k' then (k',f v):kvs else (k',v):update f k kvs


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
      test lib (Apply "exp" [Const 2, Var "n"]) "n" 3
      print $ peval (lib,  Apply "mod" [Const 8, Const 3])
      test lib (Apply "mod" [Var "x", Const 3]) "x" 8
