module Main where

import Syntax
import Execution

main =
	print $ exec myBlock [] [] [(1,0)]
