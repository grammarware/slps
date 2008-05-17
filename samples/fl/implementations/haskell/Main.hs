module Main where

import System
import Control.Monad

import Types
import Parser
import PrettyPrinter
import Evaluator
import Optimizer

toBeOptimized =
	      IfThenElse
			(Binary Equal (Literal 0) (Literal 1))
			(Literal 42)
			(Binary Plus (Literal 43) (Literal 45))

main = do
     (file1:(file2:_)) <- getArgs
     program <- parseFile file1
     ppToFile file2 program
     let (Just 120) = evaluate program (Apply "fac" [Literal 5])
     let (Literal 88) = optimize toBeOptimized
     return ()
