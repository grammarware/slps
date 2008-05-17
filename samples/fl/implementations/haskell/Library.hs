{-# OPTIONS -fglasgow-exts #-}

module Library where

import Data.Generics
import Control.Monad
import Prelude hiding (repeat)

innermost :: GenericM Maybe -> GenericT
innermost f = repeat (oncebu f)

repeat :: GenericM Maybe -> GenericT
repeat f x = maybe x (repeat f) (f x)

oncebu :: GenericM Maybe -> GenericM Maybe
oncebu f x = gmapMo (oncebu f) x `mplus` f x
