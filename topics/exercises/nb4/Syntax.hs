module Syntax where

data TrueB = TrueB
	deriving Show
data FalseB = FalseB
	deriving Show
data (NB b, NB n1, NB n2) => IfNB b n1 n2 = IfNB b n1 n2
	deriving Show
data ZeroN = ZeroN
	deriving Show
data (NB n) => SuccN n = SuccN n
	deriving Show
data (NB n) => PredN n = PredN n
	deriving Show
data (NB n) => IsZeroB n = IsZeroB n
	deriving Show

class NB x
instance NB TrueB
instance NB FalseB
instance (NB c, NB t, NB e) => NB (IfNB c t e)
instance NB ZeroN
instance (NB t) => NB (SuccN t)
instance (NB t) => NB (PredN t)
instance (NB t) => NB (IsZeroB t)
