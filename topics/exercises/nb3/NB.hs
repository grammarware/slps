module NB where

data NB
    = TrueB
    | FalseB
    | IfNB NB NB NB
	| ZeroN
	| SuccN NB
	| PredN NB
	| IsZeroB NB
    deriving Show

