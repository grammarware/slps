module ProgramAnalysis.Fix where

import DenotationalSemantics.Fix
import ProgramAnalysis.Domains


-- Equality-based fixed-point combinator

fixEq :: (Bottom x, Eq x)
      => ((x -> x) -> x -> x) -> x -> x

fixEq f x = iterate (const bottom)
 where 
  iterate r = let r' = f r  in
               if (r x == r' x) 
                 then r x
                 else iterate r'


-- An alternative formulation of fixEq

fixEq' :: (Bottom x, Eq x)
       => ((x -> x) -> x -> x) -> x -> x

fixEq' f x = findEq (repetitions bottom f x)
 where
  findEq (h:h':t)
   = if h==h'
       then h
       else findEq (h':t)
