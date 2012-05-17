-- Partial orders & Co.

module SemanticsLib.Domain (
    POrd, (<=)
  , Bottom, bottom, isBottom
  , Top, top, isTop
  , Lub, lub, lubs
  , Enumerate, enumerate
  , Proper, proper
) where

import Prelude hiding ((<=))
import qualified Data.Set


-- A type class for partial orders

class Eq x => POrd x
 where
  (<=) :: x -> x -> Bool


-- Least elements

class POrd x => Bottom x
 where
  bottom :: x

isBottom :: Bottom x => x -> Bool
isBottom = (==bottom)


-- Greatest elements

class POrd x => Top x
 where
  top :: x

isTop :: Top x => x -> Bool
isTop = (==top)


-- Least upper bounds

class POrd x => Lub x
 where
  lub :: x -> x -> x

lubs :: (Lub x, Bottom x) => [x] -> x
lubs = foldr lub bottom

{-

-- Bottom of functions based on bottom for co-domain

instance Bottom y => Bottom (x -> y)
 where
  bottom = const bottom


-- LUBs of functions based on LUBs for co-domain

instance Lub y => Lub (x -> y)
 where
  lub f g x = f x `lub` f x


-- We cannot define a partial order for function types!

instance POrd (x -> y)
 where
  (<=) = undefined


-- We cannot define equality for function types!

instance Eq (x -> y)
 where
  (==) = undefined

-}

-- Abstract values that are obtainable from concrete values

class Proper x
 where
  proper :: x -> Bool


-- Expose all the values of a (finite) domain

class Enumerate x
 where
  enumerate :: [x]


-- Instances for sets

instance Ord a => POrd (Data.Set.Set a)
 where
  (<=) = Data.Set.isSubsetOf

instance Ord a => Bottom (Data.Set.Set a)
 where
  bottom = Data.Set.empty

instance Ord a => Lub (Data.Set.Set a)
 where
  lub = Data.Set.union
