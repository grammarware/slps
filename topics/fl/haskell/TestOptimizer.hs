module Main where

import System
import Types
import ParserLib
import Parser
import Optimizer

main = do
     (file1:(file2:_)) <- getArgs
     input <- parseFile expr file1
     expected <- parseFile expr file2
     let actual = optimize input
     if actual == expected
     	then return ()
	else error "assertion error"
