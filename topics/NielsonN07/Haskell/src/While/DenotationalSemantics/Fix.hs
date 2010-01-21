-- Some reusable definitions for computing fixed points

module While.DenotationalSemantics.Fix where

import Data.Maybe


-- Fixed-point property-based fixed-point combinator

fixProperty f = f (fixProperty f)


-- Return the list of repeated function applications

repetitions b f x = [ f'i i x | i <- [0..] ]
 where
  f'i 0 = const b
  f'i i = f (f'i (i-1))


-- Iterative and Maybe-based fixed-point combinator

fixMaybe f x = findJust (repetitions Nothing f x)
 where 
  findJust = head . dropWhile isNothing
