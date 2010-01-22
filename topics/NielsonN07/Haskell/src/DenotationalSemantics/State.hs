module DenotationalSemantics.State where

import Prelude hiding (lookup)

data Eq x
  => State x n s
   = State {
       lookup :: x -> s -> n 
     , update :: x -> n -> s -> s
     } 

statesAsFunctions :: Eq x
                  => State x n (x -> n)

statesAsFunctions
 = State {
     lookup = flip ($)
   , update = \x n s x' ->
       if (x==x')
         then n
         else s x'
   } 

statesAsData :: Eq x
             => State x n [(x,n)]

statesAsData
 = State {
    lookup = \x ((x',n):s) ->
      let rec = lookup statesAsData x
       in if x' == x
            then n
            else rec s 

   , update = \x n s ->
       let rec = update statesAsData x n
        in if null s
             then [(x,n)]
             else if fst (head s) == x
               then (x,n) : tail s
               else head s : rec (tail s)
   }
