-- Partial orders & Co.

module While.ProgramAnalysis.Domains where

import Prelude hiding (Ord, (<=))


-- A type class for partial orders

class Eq x => POrd x
 where
  (<=) :: x -> x -> Bool


-- Least elements

class Bottom x
 where
  bottom :: x

isBottom :: (Eq x, Bottom x) => x -> Bool
isBottom = (==bottom)


-- Greatest elements

class Top x
 where
  top :: x

isTop :: (Eq x, Top x) => x -> Bool
isTop = (==top)


-- Least upper bounds

class Lub x
 where
  lub :: x -> x -> x


-- LUBs for "sets"

lubs :: (Lub x, Bottom x) => [x] -> x
lubs = foldr lub bottom


-- Bottom of functions based on bottom for co-domain

instance Bottom y => Bottom (x -> y)
 where
  bottom = const bottom


-- LUBs of functions based on LUBs for co-domain

instance Lub y => Lub (x -> y)
 where
  lub f g x = f x `lub` f x


-- Abstract values that are obtainable from concrete values

class Proper x
 where
  proper :: x -> Bool


-- Expose all the values of a (finite) domain

class Enumerate x
 where
  enumerate :: [x]
