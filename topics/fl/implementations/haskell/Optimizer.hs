module Optimizer where

import Types
import TraversalLib

optimize :: Data x => x -> x
optimize = innermost (mkMp step)
  where
    step (Binary Plus x (Literal 0)) = Just x
    step (Binary Plus (Literal 0) x) = Just x
    step (Binary Plus (Literal i) (Literal j)) = Just (Literal (i+j))
    step (Binary Minus x (Literal 0)) = Just x
    step (Binary Minus (Literal i) (Literal j)) = Just (Literal (i-j))
    step (Binary Equal (Literal i) (Literal j)) | i == j = Just (Literal (-1))
    step (Binary Equal (Literal i) (Literal j)) | i /= j = Just (Literal 0)
    step (IfThenElse (Literal 0) x y) = Just y
    step (IfThenElse (Literal i) x y) | i /= 0 = Just x
    step (IfThenElse x y z) | y == z = Just y
    step (IfThenElse (Binary Equal x (Literal 0)) y z) = Just (IfThenElse x z y)
    step (IfThenElse (Binary Equal (Literal 0) x) y z) = Just (IfThenElse x z y)
    step _ = Nothing
