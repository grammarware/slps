{-# OPTIONS -fglasgow-exts #-}

module Library where

import Data.Generics
import Control.Monad
import Text.ParserCombinators.ReadP
import Data.Char
import Prelude hiding (repeat)


{- ------------------------------------------------------------ -}
-- Traversal schemes defined on top of Data.Generics

innermost :: GenericM Maybe -> GenericT
innermost f = repeat (oncebu f)

repeat :: GenericM Maybe -> GenericT
repeat f x = maybe x (repeat f) (f x)

oncebu :: GenericM Maybe -> GenericM Maybe
oncebu f x = gmapMo (oncebu f) x `mplus` f x


{- ------------------------------------------------------------ -}
-- Parsing capabilities defined on top of  Text.ParserCombinators.ReadP

-- Expressions combined with left-associative infix operators

lassoc o p f = p >>= lassoc' 
  where
    lassoc' x = 
      (do 
         or <- o
  	 y <- p
         lassoc' (f or x y)) +++ (return x)


-- Parse from file

parseFile p f = do
	s <- readFile f
	let r = readP_to_S (complete p) s
	if (length r == 1)
	   then return (fst (head r))
	   else fail "parse error"
  where
    -- Complete parse
    complete p = p >>= \r -> eof >> return r
    -- Test for end of file
    eof = munch isSpace >> look >>= guard . ((==) "")


-- Load-ahead test

follows p = 
  do
	x <- look
	guard (not (null x) && (p (head x)))


{- ------------------------------------------------------------ -}
