{-# OPTIONS -fglasgow-exts #-}

module TraversalLib (
  module Data.Generics,
  innermost
) where

import Data.Generics
import Control.Monad
import Prelude hiding (repeat)


-- Traversal schemes defined on top of Data.Generics

bottomup :: Data x => (forall y. Data y => y -> y) -> x -> x
bottomup f = f .  gmapT (bottomup f) 

innermost :: GenericM Maybe -> GenericT
innermost f = repeat (oncebu f)

repeat :: GenericM Maybe -> GenericT
repeat f x = maybe x (repeat f) (f x)

oncebu :: GenericM Maybe -> GenericM Maybe
oncebu f x = gmapMo (oncebu f) x `mplus` f x
