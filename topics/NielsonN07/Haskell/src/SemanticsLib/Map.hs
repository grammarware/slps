-- Maps as partial orders with values subject to partial order too

module SemanticsLib.Map (
    Map
  , fromList, toList
  , lookup, update
  , statesAsPOrdMaps
  , keys, maps, atomic
) where

import Prelude hiding (Ord, (<=), lookup)
import SemanticsLib.State (StateAlg(StateAlg))
import qualified SemanticsLib.State as State
import SemanticsLib.Domain


-- A po-set of functions as lists of key-value pairs

newtype ( Eq k
        , Bottom v
        )
          => Map k v 
           = Map { getMap :: [(k,v)] }


-- List interface

fromList :: (Eq k, Bottom v) => [(k,v)] -> Map k v
fromList = foldr (\(k,v) m -> update k v m) bottom 

toList :: (Eq k, Bottom v) => Map k v -> [(k,v)] 
toList = getMap


-- States as partially ordered maps

statesAsPOrdMaps :: (Eq k, Bottom v)
                 => StateAlg k v (Map k v)

statesAsPOrdMaps = StateAlg {
   State.lookup = lookup
 , State.update = update
}


-- Show instance for maps

instance (Eq k, Bottom v, Show k, Show v) => Show (Map k v)
 where
  show = show . getMap


-- Function application

lookup :: (Eq k, Bottom v)
       => k -> Map k v -> v

lookup _ (Map []) = bottom
lookup k (Map ((k',v):m))
 = if (k == k')
     then v
     else lookup k (Map m)


-- Point-wise function modification

update :: (Eq k, Bottom v)
       => k -> v -> Map k v -> Map k v

update k v m
 = if isBottom v
     then m
     else update' m
 where
  update' (Map [])          = Map $ (k,v) : []
  update' (Map ((k',v'):m)) = 
    if (k == k')
      then Map $ (k,v) : m
      else Map $ (k',v')  : getMap (update' (Map m))


-- The bottom element of maps

instance (Eq k, Bottom v)
      => Bottom (Map k v)
 where
  bottom = Map []


-- Equality on maps

instance (Eq k, Bottom v)
      => Eq (Map k v)
 where
  m1 == m2 = m1 <= m2 && m2 <= m1


-- Partial order on maps

instance (Eq k, Bottom v)
      => POrd (Map k v)
 where
  (Map [])        <= _  = True
  (Map ((k,v):m)) <= m' =    v     <= lookup k m'
                          && Map m <= m'


-- Pointwise LUB for maps

instance (Eq k, Bottom v, Lub v)
      => Lub (Map k v)
 where
  (Map [])        `lub` m' = m'
  (Map ((k,v):m)) `lub` m' = update k v' m''
   where
    v' = v `lub` lookup k m'
    m'' = (Map m) `lub` m'


-- Return the domain of the function

keys :: (Eq k, Bottom v) => Map k v -> [k]
keys = map fst . getMap


-- Return all possible maps within bounds

maps :: (Eq k, Bottom v, Enumerate v)
     => [k] -> [Map k v]

maps [] = [bottom]
maps (k:ks) = concat (map f enumerate)
 where
  f v = map (update k v) ms
  ms = maps ks


-- Require values to be proper for all given keys

atomic :: (Eq k, Bottom v, Proper v)
       => [k] -> Map k v -> Bool

atomic ks m = and $ map f ks
 where
  f k = proper $ lookup k m
