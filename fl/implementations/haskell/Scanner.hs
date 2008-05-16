module Scanner where

import Text.ParserCombinators.ReadP
import Data.Char hiding (isSpace)
import Control.Monad


-- Predicates for character sets

isSpace = (==) ' '
isEoln = (==) '\n'
isWhitespace x = isSpace x || isEoln x
-- Others are imported from Data.Char


-- Token classes with withspace handling

spaces = munch isSpace

int = spaces >> munch1 isDigit

name = spaces >> munch1 isLower

special s = spaces >> string s

eoln = spaces >> satisfy isEoln

eof = spaces >> look >>= guard . ((==) "")

keyword s = spaces >> string s >> (follows isWhitespace <++ eof)


-- Test look-ahead character

follows p = 
  do
	x <- look
	guard (not (null x) && (p (head x)))
