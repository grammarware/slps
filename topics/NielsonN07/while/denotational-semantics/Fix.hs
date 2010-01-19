{-# LANGUAGE FlexibleContexts #-}

module Fix where

import Ordering
import Data.Maybe


-- Recursive fixed-point combinator

fixRecursive :: (t -> t) -> t
fixRecursive f = f (fixRecursive f)


-- Return the list of repeated function applications

repetitions b f x = [ f'i i x | i <- [0..] ]
 where
  f'i 0 = const b
  f'i i = f (f'i (i-1))


-- Iterative fixed-point combinator based on Maybe

fixMaybe :: ((x -> Maybe x) -> x -> (Maybe x)) -> x -> (Maybe x)
fixMaybe f x = findJust (repetitions Nothing f x)
 where 
  findJust = head . dropWhile isNothing


-- Iterative fixed-point combinator based on equality

fixEq :: (Bottom x, Eq x)
      => ((x -> x) -> x -> x) -> x -> x
fixEq f x = findEq (repetitions bottom f x)
 where
  findEq (h:h':t)
   = if h==h'
       then h
       else findEq (h':t)
