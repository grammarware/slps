module Main where

import Syntax
import Execution

main =
	do
		print $ exec myWhile []
