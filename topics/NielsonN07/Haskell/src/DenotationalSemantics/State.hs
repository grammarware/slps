module DenotationalSemantics.State where

import Prelude hiding (lookup)

data Eq x
  => State x n s
   = State {
       lookup :: x -> s -> n 
     , update :: x -> n -> s -> s
     } 

statesAsFunctions :: Eq x => State x n (x -> n)
statesAsFunctions
 = State {
     lookup = flip ($)
   , update = \x n s x' ->
       if (x==x')
         then n
         else s x'
   } 

statesAsListsOfPairs :: Eq x => State x n [(x,n)]
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
