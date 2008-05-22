module Main where

import System
import Types
import ParserLib
import Parser
import PrettyPrinter

main = do
     (file1:(file2:_)) <- getArgs
     program <- parseFile program file1
     ppToFile file2 program
