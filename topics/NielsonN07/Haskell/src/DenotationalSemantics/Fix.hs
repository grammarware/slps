-- Some reusable definitions for computing fixed points

module DenotationalSemantics.Fix where

import Data.Maybe


-- Fixed-point property-based fixed-point combinator

fixProperty :: (x -> x) -> x
fixProperty f = f (fixProperty f)


-- Iterative and Maybe-based fixed-point combinator

fixMaybe :: ((x -> Maybe x) -> (x -> Maybe x)) -> x -> Maybe x
fixMaybe f x = iterate (const Nothing)
 where 
  iterate g = maybe (iterate (f g)) Just (g x)


-- An alternative formulation of fixMaybe

fixMaybe' :: ((x -> Maybe x) -> (x -> Maybe x)) -> x -> Maybe x
fixMaybe' f x = findJust (repetitions Nothing f x)
 where 
  findJust = head . dropWhile isNothing


-- Return the list of repeated function applications

repetitions :: a -> ((b -> a) -> b -> a) -> b -> [a]
repetitions b f x = [ f'i i x | i <- [0..] ]
 where
  f'i 0 = const b
  f'i i = f (f'i (i-1))
