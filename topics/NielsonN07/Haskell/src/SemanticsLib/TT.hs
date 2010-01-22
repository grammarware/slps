-- A complete lattice of truth values

module SemanticsLib.TT (
    TT(BottomTT, TT, FF, TopTT)
  , ttBooleans
  , EqTT, (.==.)
  , OrdTT, (.<=.)
) where

import Prelude hiding (Ord, (<=), not, (&&), and)
import SemanticsLib.Boolean
import SemanticsLib.Domain


-- The data type for truth values

data TT = BottomTT
        | TT 
        | FF
        | TopTT
 deriving (Eq, Show)


-- Abstract standard Booleans as tt Booleans

bool2tt :: Bool -> TT
bool2tt True = TT
bool2tt False = FF

ttBooleans :: BooleanAlg TT
ttBooleans  = BooleanAlg {
    fromBool = bool2tt
  , true     = TT
  , false    = FF
  , not      = notTT
  , and      = andTT
}


-- Negation

notTT :: TT -> TT
notTT BottomTT = bottom
notTT TT       = FF
notTT FF       = TT
notTT TopTT    = top


-- Conjunction

andTT :: TT -> TT -> TT
andTT BottomTT _        = bottom
andTT _        BottomTT = bottom
andTT TT       TT       = TT
andTT FF       _        = FF
andTT _        FF       = FF
andTT _        _        = top


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


-- Ordering on truth values

instance POrd TT
 where
  BottomTT <= _     = True
  _        <= TopTT = True
  b1       <= b2    = b1 == b2


-- Least and greatest elements, and LUBs

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
