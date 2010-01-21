module While.DenotationalSemantics.State where

import Prelude hiding (lookup)
import While.AbstractSyntax (Var)

data State n s
   = State {
       lookup :: Var -> s -> n 
     , update :: Var -> n -> s -> s
     } 

statesAsFunctions :: State n (Var -> n)
statesAsFunctions
 = State {
     lookup = flip ($)
   , update = \x n s x' ->
       if (x==x')
         then n
         else s x'
   } 

statesAsListsOfPairs :: State n [(Var,n)]
statesAsListsOfPairs
 = State {
    lookup = \x ((x',n):s) ->
      let rec = lookup statesAsListsOfPairs x
       in if x' == x
            then n
            else rec s 

   , update = \x n s ->
       let rec = update statesAsListsOfPairs x n
        in if null s
             then [(x,n)]
             else if fst (head s) == x
               then (x,n) : tail s
               else head s : rec (tail s)
   }
