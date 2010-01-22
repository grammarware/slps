-- The domain of states
-- Aka variable assignments

module SemanticsLib.State (
    StateAlg(StateAlg)
  , lookup
  , update
  , statesAsFunctions
  , statesAsData
) where

import Prelude hiding (lookup)

data Eq x
  => StateAlg x n s
   = StateAlg {
       lookup :: x -> s -> n 
     , update :: x -> n -> s -> s
     } 

statesAsFunctions :: Eq x
                  => StateAlg x n (x -> n)

statesAsFunctions
 = StateAlg {
     lookup = flip ($)
   , update = \x n s x' ->
       if (x==x')
         then n
         else s x'
   } 

statesAsData :: Eq x
             => StateAlg x n [(x,n)]

statesAsData
 = StateAlg {
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
