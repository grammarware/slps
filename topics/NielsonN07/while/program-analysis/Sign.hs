-- Abstract numbers (signs)

module Sign where

import Prelude
import Ordering
import TT


-- The data type for signs

data Sign = BottomSign
          | Zero | Pos | Neg
          | TopSign
 deriving (Eq, Show)


-- Ordering on signs

instance Ord Sign
 where
  BottomSign  <= _       = True
  _           <= TopSign = True
  s1          <= s2      = s1 == s2


-- The complete lattice of signs

instance Bottom Sign
 where
  bottom = BottomSign

instance Top Sign
 where
  top = TopSign

instance Lub Sign
 where
  s1 `lub` s2 = if s1 <= s2 then s2 else
                if s2 <= s1 then s1 else
                top


-- Signs as Haskell-like numbers

instance Num Sign
 where

  signum = id

  abs BottomSign = BottomSign
  abs TopSign    = TopSign
  abs Zero       = Zero
  abs Pos        = Pos
  abs Neg        = Pos

  fromInteger n | n  > 0    = Pos
                | n  < 0    = Neg
                | otherwise = Zero

  BottomSign + _          = bottom
  _          + BottomSign = bottom
  TopSign    + _          = top
  _          + TopSign    = top
  Zero       + Zero       = Zero
  Zero       + Pos        = Pos
  Zero       + Neg        = Neg
  Pos        + Zero       = Pos
  Pos        + Pos        = Pos
  Pos        + Neg        = top
  Neg        + Zero       = Neg
  Neg        + Pos        = top
  Neg        + Neg        = Neg

  BottomSign * _          = bottom
  _          * BottomSign = bottom
  TopSign    * _          = top
  _          * TopSign    = top
  Zero       * Zero       = Zero
  Zero       * Pos        = Zero
  Zero       * Neg        = Zero
  Pos        * Zero       = Zero
  Pos        * Pos        = Pos
  Pos        * Neg        = Neg
  Neg        * Zero       = Zero
  Neg        * Pos        = Neg
  Neg        * Neg        = Pos

  BottomSign - _          = bottom
  _          - BottomSign = bottom
  TopSign    - _          = top
  _          - TopSign    = top
  Zero       - Zero       = Zero
  Zero       - Pos        = Neg
  Zero       - Neg        = Pos
  Pos        - Zero       = Pos
  Pos        - Pos        = top
  Pos        - Neg        = Pos
  Neg        - Zero       = Neg
  Neg        - Pos        = Neg
  Neg        - Neg        = top


-- Abstraction-based equality on signs

instance EqTT Sign
 where
  BottomSign .==. _          = bottom
  _          .==. BottomSign = bottom
  TopSign    .==. _          = top
  _          .==. TopSign    = top
  Pos        .==. Pos        = top
  Neg        .==. Neg        = top
  Zero       .==. Zero       = TT
  _          .==. _          = FF


-- Abstraction-based ordering on signs

instance OrdTT Sign
 where
  BottomSign .<=. _          = bottom
  _          .<=. BottomSign = bottom
  TopSign    .<=. _          = top
  _          .<=. TopSign    = top
  Pos        .<=. Pos        = top
  Neg        .<=. Neg        = top
  Pos        .<=. Zero       = FF
  Pos        .<=. Neg        = FF
  Zero       .<=. Neg        = FF
  _          .<=. _          = TT


-- Expose all values

instance Values Sign
 where
  values = [BottomSign, Zero, Pos, Neg, TopSign]


-- Test for proper values

instance Proper Sign
 where
  proper BottomSign = False
  proper TopSign    = False
  proper _          = True
