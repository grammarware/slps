{-# OPTIONS -fglasgow-exts #-}

module ParserLib (
  module Text.ParserCombinators.ReadP,
  module Data.Char,
  lassoc,
  parseFile,
  follows
) where

import Control.Monad
import Text.ParserCombinators.ReadP
import Data.Char

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
