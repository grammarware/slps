module Execution where

import Syntax
import Evaluation
import Prelude hiding (lookup)

update :: Environment -> Store -> Identifier -> Int -> Store
update [] sto _ _ = sto
update ((x1,l):xls) sto x2 v = match x1 x2 (updateStore sto l v) (update xls sto x2 v)
	
updateStore :: Store -> Location -> Int -> Store	
updateStore [] l v = []
updateStore ((l1,v1):lvs) l2 v2 = match l1 l2 ((l1,v2):lvs) ((l1,v1):updateStore lvs l2 v2)

-- Denotational semantics of Extended While, slides 545–554
exec :: Statement -> Environment -> PEnvironment -> (Store -> Store)
exec (Seq s1 s2) env penv = exec s2 env penv . exec s1 env penv
exec Skip _ _ = id
exec (IfThenElse b s1 s2) env penv = cond (evalb b env) (exec s1 env penv) (exec s2 env penv)
exec (While b s) env penv = fix f
	where
		f g = cond (evalb b env) (g . exec s env penv) id
exec (Call x) env penv = getProc penv x
exec (Assign x a) env penv = h
	where
		h sto = update env sto x (evala a env sto)
exec (Block dv dp s) env penv = h
	where
		h sto = exec s env' penv' sto'
			where 
				(env', sto') = decVar dv (env, sto)
				penv' = decProc dp env' penv

cond p g1 g2 sto | p sto      = g1 sto
                 | otherwise = g2 sto

fix :: (t -> t) -> t
fix f = f (fix f)  

-- slides 550, 551
decVar :: [VariableDeclaration] -> (Environment, Store) -> (Environment, Store)
decVar [] = id
decVar ((Var x a):dv) = h
	where
		h (env, sto) = decVar dv (modifyEnvironment env x l, modifyNext (modifySto sto l (evala a env sto)) l)
			where
				l = fst (head sto)

matchNmodify xys x y =
	if null xys
		then [(x,y)]
		else match (fst (head xys)) x ((x,y):xys) (head xys:matchNmodify (tail xys) x y)

modifyEnvironment :: Environment -> Identifier -> Location -> Environment
modifyEnvironment [] x l = [(x,l)]
modifyEnvironment ((x1,l1):xls) x2 l2 = match x1 x2 ((x1,l2):xls) ((x1,l1):modifyEnvironment xls x2 l2)
	
modifySto :: Store -> Location -> Int -> Store
modifySto [] l v = [(l,v)]
modifySto ((l1,v1):lvs) l2 v2 = match l1 l2 ((l1,v2):lvs) ((l1,v1):modifySto lvs l2 v2)
	
modifyNext :: Store -> Location -> Store
modifyNext lvs l = (l+1,0):lvs

-- slides 552–554
decProc :: [ProcedureDeclaration] -> Environment -> PEnvironment -> PEnvironment
decProc [] env = id
decProc ((Proc p s):dp) env = h
	where 
		h penv = decProc dp env (modifyPEnvironment penv p (fix (exec s env . modifyPEnvironment penv p)))

modifyPEnvironment :: PEnvironment -> Procedure -> (Store -> Store) -> PEnvironment
modifyPEnvironment [] p f = [(p,f)]
modifyPEnvironment ((p1,f1):pfs) p2 f2 = match p1 p2 ((p1,f2):pfs) ((p1,f1):modifyPEnvironment pfs p2 f2)

getProc :: Eq a => [(a,Store -> Store)] -> a -> (Store -> Store)
getProc [] a = id
getProc ((a1,b):abs) a2 = match a1 a2 b (getProc abs a2)
