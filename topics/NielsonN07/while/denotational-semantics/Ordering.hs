module Ordering where


-- Least elements

class Ord x => Bottom x
 where
  bottom   :: x

isBottom :: Bottom x => x -> Bool
isBottom = (==bottom)


-- Greatest elements

class Ord x => Top x
 where
  top   :: x

isTop :: Top x => x -> Bool
isTop = (==top)


-- Least upper bounds

class Ord x => Lub x
 where
  lub :: x -> x -> x


-- LUBs for "sets"

lubs :: (Lub x, Bottom x) => [x] -> x
lubs = foldr lub bottom


-- Expose all the values of a (finite) domain

class Values x
 where
  values :: [x]


-- Elements that are not bottoms or tops

class Proper x
 where
  proper :: x -> Bool
