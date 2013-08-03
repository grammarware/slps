-- The complete lattice of abstract numbers for signs

module Sign (
    Sign(BottomSign, Zero, Pos, Neg, TopSign, ZeroPos, ZeroNeg, One)
  , signNumbers
) where


import Prelude hiding (Ord, (<=))
import SemanticsLib.Number
import SemanticsLib.Domain
import SemanticsLib.TT


-- The data type for signs

data Sign = BottomSign
          | Zero
          | Pos
          | Neg
          | One         -- added
          | ZeroPos		-- a.k.a. NotNeg
          | ZeroNeg		-- a.k.a. NotPos
          | TopSign
 deriving (Eq, Show)


-- Signs as numbers

signNumbers :: NumberAlg Sign TT
signNumbers  = NumberAlg {
   from = fromInteger
 , add = (+)
 , mul = (*)
 , sub = (-)
 , eq  = (.==.)
 , leq = (.<=.)
}


-- Signs as Haskell-like numbers

instance Num Sign
 where

  signum = id

  abs BottomSign = BottomSign
  abs TopSign    = TopSign
  abs Zero       = Zero
  abs Pos        = Pos
  abs Neg        = Pos
  abs One        = One							-- added
  abs ZeroPos    = ZeroPos						-- added
  abs ZeroNeg    = ZeroPos						-- added

  fromInteger n | n == 1    = One				-- added
                | n == 0    = Zero				-- changed
                | n  > 0    = Pos
                | n  < 0    = Neg

  BottomSign + _          = bottom
  _          + BottomSign = bottom
  TopSign    + _          = top
  _          + TopSign    = top
  Zero       + Zero       = Zero
  Zero       + Pos        = Pos
  Zero       + Neg        = Neg
  Zero       + One        = One					-- added
  Zero       + ZeroPos    = ZeroPos				-- added
  Zero       + ZeroNeg    = ZeroNeg				-- added
  Pos        + Zero       = Pos
  Pos        + Pos        = Pos
  Pos        + Neg        = top
  Pos        + One        = One					-- added
  Pos        + ZeroPos    = Pos					-- added
  Pos        + ZeroNeg    = top					-- added
  Neg        + Zero       = Neg
  Neg        + Pos        = top
  Neg        + Neg        = Neg
  Neg        + One        = ZeroNeg				-- added
  Neg        + ZeroPos    = top					-- added
  Neg        + ZeroNeg    = Neg					-- added
--------
  One        + Zero       = One					-- added
  One        + Pos        = Pos					-- added
  One        + Neg        = ZeroNeg				-- added
  One        + One        = Pos					-- added
  One        + ZeroPos    = Pos					-- added
  One        + ZeroNeg    = top					-- added
--------
  ZeroPos    + Zero       = ZeroPos				-- added
  ZeroPos    + Pos        = Pos					-- added
  ZeroPos    + Neg        = top					-- added
  ZeroPos    + One        = Pos					-- added
  ZeroPos    + ZeroPos    = ZeroPos				-- added 
  ZeroPos    + ZeroNeg    = top					-- added
--------
  ZeroNeg    + Zero       = ZeroNeg				-- added
  ZeroNeg    + Pos        = top					-- added
  ZeroNeg    + Neg        = Neg					-- added
  ZeroNeg    + One        = top					-- added
  ZeroNeg    + ZeroPos    = top					-- added 
  ZeroNeg    + ZeroNeg    = top					-- added

  BottomSign * _          = bottom
  _          * BottomSign = bottom
  TopSign    * _          = top
  _          * TopSign    = top
  Zero       * Zero       = Zero
  Zero       * Pos        = Zero
  Zero       * Neg        = Zero
  Zero       * One        = Zero				-- added
  Zero       * ZeroPos    = Zero				-- added
  Zero       * ZeroNeg    = Zero				-- added
  Pos        * Zero       = Zero
  Pos        * Pos        = Pos
  Pos        * Neg        = Neg
  Pos        * One        = Pos					-- added
  Pos        * ZeroPos    = ZeroPos				-- added
  Pos        * ZeroNeg    = ZeroNeg				-- added
  Neg        * Zero       = Zero
  Neg        * Pos        = Neg
  Neg        * Neg        = Pos
  Neg        * One        = ZeroNeg				-- added
  Neg        * ZeroPos    = ZeroNeg				-- added
  Neg        * ZeroNeg    = ZeroPos				-- added
--------
  One        * Zero       = Zero				-- added
  One        * Pos        = Pos					-- added
  One        * Neg        = Neg					-- added
  One        * One        = One					-- added
  One        * ZeroPos    = ZeroPos				-- added
  One        * ZeroNeg    = top					-- added
--------
  ZeroPos    * Zero       = ZeroPos				-- added
  ZeroPos    * Pos        = Pos					-- added
  ZeroPos    * Neg        = top					-- added
  ZeroPos    * One        = Pos					-- added
  ZeroPos    * ZeroPos    = ZeroPos				-- added 
  ZeroPos    * ZeroNeg    = top					-- added
--------
  ZeroNeg    * Zero       = ZeroNeg				-- added
  ZeroNeg    * Pos        = ZeroNeg				-- added
  ZeroNeg    * Neg        = ZeroPos				-- added
  ZeroNeg    * One        = ZeroNeg				-- added
  ZeroNeg    * ZeroPos    = ZeroNeg				-- added 
  ZeroNeg    * ZeroNeg    = ZeroPos				-- added

  BottomSign - _          = bottom
  _          - BottomSign = bottom
  TopSign    - _          = top
  _          - TopSign    = top
  Zero       - Zero       = Zero
  Zero       - Pos        = Neg
  Zero       - Neg        = Pos
  Zero       - One        = Neg					-- added
  Zero       - ZeroPos    = ZeroNeg				-- added
  Zero       - ZeroNeg    = ZeroPos				-- added
  Pos        - Zero       = Pos
  Pos        - Pos        = top
  Pos        - Neg        = Pos
  Pos        - One        = ZeroPos				-- added
  Pos        - ZeroPos    = top					-- added
  Pos        - ZeroNeg    = Pos					-- added
  Neg        - Zero       = Neg
  Neg        - Pos        = top
  Neg        - Neg        = Neg
  Neg        - One        = Neg					-- added
  Neg        - ZeroPos    = Neg					-- added
  Neg        - ZeroNeg    = top					-- added
--------
  One        - Zero       = One					-- added
  One        - Pos        = ZeroNeg				-- added
  One        - Neg        = ZeroPos				-- added
  One        - One        = Zero				-- added
  One        - ZeroPos    = Neg					-- added
  One        - ZeroNeg    = Pos					-- added
--------
  ZeroPos    - Zero       = ZeroPos				-- added
  ZeroPos    - Pos        = top					-- added
  ZeroPos    - Neg        = Pos					-- added
  ZeroPos    - One        = Pos					-- added
  ZeroPos    - ZeroPos    = top					-- added 
  ZeroPos    - ZeroNeg    = ZeroPos				-- added
--------
  ZeroNeg    - Zero       = ZeroNeg				-- added
  ZeroNeg    - Pos        = Neg					-- added
  ZeroNeg    - Neg        = top					-- added
  ZeroNeg    - One        = Neg					-- added
  ZeroNeg    - ZeroPos    = ZeroNeg				-- added 
  ZeroNeg    - ZeroNeg    = top					-- added


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
  One        .==. One        = TT				-- added
  ZeroPos    .==. ZeroPos    = top				-- added
  ZeroNeg    .==. ZeroNeg    = top				-- added
  One        .==. Pos        = top				-- added
  One        .==. ZeroPos    = top				-- added
  Zero       .==. ZeroPos    = top				-- added
  Zero       .==. ZeroNeg    = top				-- added
  _          .==. _          = FF


-- Abstraction-based ordering on signs

instance OrdTT Sign
 where
  BottomSign .<=. _          = bottom
  _          .<=. BottomSign = bottom
  TopSign    .<=. _          = top
  _          .<=. TopSign    = top
-- based on the table for ≤, we expect top 15 times, TT 14 times and FF 7 times
-- all that’s true:
  Neg        .<=. Pos        = TT
  Neg        .<=. Zero       = TT
  Neg        .<=. ZeroPos    = TT
  Neg        .<=. One        = TT
  Zero       .<=. Pos        = TT
  Zero       .<=. Zero       = TT
  Zero       .<=. ZeroPos    = TT
  Zero       .<=. One        = TT
  ZeroNeg    .<=. Pos        = TT
  ZeroNeg    .<=. Zero       = TT
  ZeroNeg    .<=. ZeroPos    = TT
  ZeroNeg    .<=. One        = TT
  One        .<=. Pos        = TT
  One        .<=. One        = TT
-- all that’s false:
  Pos        .<=. Neg        = FF
  Pos        .<=. Zero       = FF
  Pos        .<=. ZeroNeg    = FF
  Zero       .<=. Neg        = FF
  ZeroPos    .<=. Neg        = FF
  One        .<=. Neg        = FF
  One        .<=. Zero       = FF
-- the rest is anything
  _          .<=. _          = top

-- Ordering on signs

instance POrd Sign
 where
  BottomSign  <= _       = True
  _           <= TopSign = True
-- see the Hasse diagram
  One         <= Pos     = True
  One         <= ZeroPos = True
  Zero        <= ZeroPos = True
  Pos         <= ZeroPos = True
  Zero        <= ZeroNeg = True
  Neg         <= ZeroNeg = True
  s1          <= s2      = s1 == s2


-- Least and greatest elements, and LUBs

instance Bottom Sign
 where
  bottom = BottomSign

instance Top Sign
 where
  top = TopSign

instance Lub Sign
 where
  s1 `lub` s2 = if null upper then top
                else head upper
                  where upper = [x | x <- [Zero, One, Pos, Neg, ZeroPos, ZeroNeg], s1 <= x, s2 <= x]


-- Expose all values of Sign

instance Enumerate Sign
 where
  enumerate = [BottomSign, Zero, Pos, Neg, TopSign, ZeroPos, ZeroNeg, One]


-- Test for proper values of Sign

instance Proper Sign
 where
  proper BottomSign = False
  proper TopSign    = False
  proper _          = True
