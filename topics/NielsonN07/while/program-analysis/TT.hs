-- Abstract truth values

module TT where

import Ordering


-- The data type for truth values

data TT = BottomTT
        | TT 
        | FF
        | TopTT
 deriving (Eq, Show)


-- Ordering on truth values

instance Ord TT
 where
  BottomTT <= _     = True
  _        <= TopTT = True
  b1       <= b2    = b1 == b2


-- The complete lattice of truth values

instance Bottom TT
 where
  bottom = BottomTT

instance Top TT
 where
  top = TopTT

instance Lub TT
 where
  b1 `lub` b2 = if b1 <= b2 then b2 else
                if b2 <= b1 then b1 else
                top


-- Negation

not :: TT -> TT
not BottomTT = bottom
not TT       = FF
not FF       = TT
not TopTT    = top


-- Conjunction

(&&) :: TT -> TT -> TT

BottomTT && _        = bottom
_        && BottomTT = bottom
TT       && TT       = TT
FF       && _        = FF
_        && FF       = FF
_        && _        = top


-- Equality relative to TT

infix 4 .==.

class EqTT x
 where
  (.==.) :: x -> x -> TT


-- Less-or-equal-than relative to TT

infix 4 .<=.

class OrdTT x
 where
  (.<=.) :: x -> x -> TT
