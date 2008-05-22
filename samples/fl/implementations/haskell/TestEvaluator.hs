module Main where

import System
import Types
import ParserLib
import Parser
import Evaluator

main = do
     (file1:(file2:(expected:_))) <- getArgs
     program <- parseFile program file1
     expr <- parseFile expr file2
     let actual = evaluate program expr
     if actual == Just (read expected)
     	then return ()
	else error "assertion error"
