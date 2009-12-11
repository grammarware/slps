module Exec where

import Syntax
import Eval
import Prelude hiding (lookup)

update :: State -> Identifier -> Int -> State
update [] x v = [(x, v)]
update ((x1,v1):xvs) x2 v2 =
    if x1 == x2
    then ((x1,v2):xvs)
    else ((x1,v1):(update xvs x2 v2))

exec :: Statement -> State -> State
exec (Seq s1 s2) = exec s2 . exec s1
exec Skip = id
exec (Assign x a) = \st -> update st x (evala a st)
exec (IfThenElse b s1 s2) = cond (evalb b) (exec s1) (exec s2) 
exec (While b s) = fix f
 where
  f g = cond (evalb b) (g . exec s) id

cond p g1 g2 st | p st      = g1 st
                | otherwise = g2 st

fix :: (t -> t) -> t
fix f = f (fix f)

main =
	do
		print $ exec myWhile []
