module Evaluation where
import Syntax

data Result
	= Bool Bool
	| Num Int
	| Neither
	deriving Show

class NB x => Evaluate x
	where eval :: x -> Result

instance Evaluate TrueB
	where eval TrueB = Bool True

instance Evaluate FalseB
	where eval FalseB = Bool False

instance (Evaluate c, Evaluate t, Evaluate e) => Evaluate (IfNB c t e)
	where eval (IfNB c t e) = case (eval c) of
		Bool x -> if x then eval t else eval e
		_ -> Neither

instance Evaluate ZeroN
	where eval ZeroN = Num 0

instance (Evaluate t) => Evaluate (SuccN t)
	where eval (SuccN t) = case (eval t) of
		Num x -> Num (x+1)
		_ -> Neither

instance (Evaluate t) => Evaluate (PredN t)
	where eval (PredN t) = case (eval t) of
		Num x -> Num (x-1)
		_ -> Neither

instance (Evaluate t) => Evaluate (IsZeroB t)
	where eval (IsZeroB t) = case (eval t) of
		Num x -> Bool (x==0)
		_ -> Neither
