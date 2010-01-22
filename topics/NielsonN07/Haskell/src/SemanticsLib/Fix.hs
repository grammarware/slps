-- Some reusable definitions for computing fixed points

module SemanticsLib.Fix (
    fixProperty
  , fixMaybe
  , fixEq
) where

import Data.Maybe
import SemanticsLib.Domain


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
