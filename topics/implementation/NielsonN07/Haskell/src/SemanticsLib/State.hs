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
  => StateAlg x v s
   = StateAlg {
       lookup :: x -> s -> v 
     , update :: x -> v -> s -> s
     } 

statesAsFunctions :: Eq x
                  => StateAlg x v (x -> v)

statesAsFunctions
 = StateAlg {
     lookup = flip ($)
   , update = \x v s x' ->
       if (x==x')
         then v
         else s x'
   } 

statesAsData :: Eq x
             => StateAlg x v [(x,v)]

statesAsData
 = StateAlg {
    lookup = \x ((x',v):s) ->
      let rec = lookup statesAsData x
       in if x' == x
            then v
            else rec s 

   , update = \x v s ->
       let rec = update statesAsData x v
        in if null s
             then [(x,v)]
             else if fst (head s) == x
               then (x,v) : tail s
               else head s : rec (tail s)
   }
