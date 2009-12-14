module FoldNB where

import NB

foldNB :: r -> r -> (r -> r -> r -> r) -> r -> (r -> r) -> (r -> r) -> (r -> r) -> (NB -> r)
--     TrueB FalseB       IfNB          ZeroN    SuccN      PredN      IsZeroB
foldNB r _ _ _ _ _ _ TrueB = r
foldNB _ r _ _ _ _ _ FalseB = r
foldNB t f i z s p r (IfNB a b c) =
	i (fold a) (fold b) (fold c)
	where fold = foldNB t f i z s p r
foldNB _ _ _ r _ _ _ ZeroN = r
foldNB t f i z s p r (SuccN a) =
	s (fold a)
	where fold = foldNB t f i z s p r
foldNB t f i z s p r (PredN a) =
	p (fold a)
	where fold = foldNB t f i z s p r
foldNB t f i z s p r (IsZeroB a) =
	r (fold a)
	where fold = foldNB t f i z s p r

depth :: NB -> Int
depth = foldNB
	1
	1
	(\ x y z -> 1 + (maximum [x, y, z]))
	1
	(+1)
	(+1)
	(+1)


countT :: NB -> Int
countT = foldNB
	1
	0
	(\ x y z -> x + y + z)
	0
	(const 0)
	(const 0)
	(const 0)

maxN :: NB -> Int
maxN = foldNB
	0
	0
	(\ x y z -> max y z)
	0
	(+1)
	(\ x -> if x>1 then x-1 else 0)
	(id)

evaln :: NB -> Int
evaln = foldNB
	1
	0
	(\ x y z -> if x/=0 then y else z)
	0
	(+1)
	(\ x -> if x>1 then x-1 else 0)
	(\ x -> if x==0 then 1 else 0)

data Answer = Num Int | Bool Bool deriving Show

evalnb :: NB -> Answer
evalnb = foldNB
	(Bool True)
	(Bool False)
	(\ (Bool x) y z -> if x then y else z)
	(Num 0)
	(\ (Num x) -> Num (x+1) )
	(\ (Num x) -> if x>1 then (Num (x-1)) else Num 0)
	(\ (Num x) -> if x==0 then Bool True else Bool False)
