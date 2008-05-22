module Scanner where

import Text.ParserCombinators.ReadP
import Data.Char
import Control.Monad
import Library


-- Withspace excluding eoln

spaces = munch (==' ')


-- Keywords of FL

isKeyword("if") = True
isKeyword("then") = True
isKeyword("else") = True
isKeyword(_) = False

keyword s = spaces >> string s >> follows isSpace


-- Special characters

special s = spaces >> string s


-- Eoln token

eoln = spaces >> satisfy (=='\n')


-- Integers with optional negative sign

int = 
  do
	spaces
	sign <- option ' ' (char '-')
	digits <- munch1 isDigit
	return (readInt (sign:digits))
  where
    readInt :: String -> Int
    readInt = read


-- Names that are not keywords

name =
  do
	spaces
	n <- munch1 isLower
	guard (not (isKeyword n))
	return n
