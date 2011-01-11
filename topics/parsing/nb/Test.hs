module Main where

import ParserLib
import Parser
import AbstractSyntax


main = do
	print $ fst $ head $ parse nb "0"
	print $ fst $ head $ parse nb "0"
	print $ fst $ head $ parse nb "if true then false else if false then true else false fi fi"
	print $ fst $ head $ parse nb "if true then false else if false then true else false fi fi"
	print $ fst $ head $ parse nb "if is pred(succ(0)) zero? then true else false fi"
	print $ fst $ head $ parse nb "if is pred(succ(0)) zero? then true else false fi"
	print $ fst $ head $ parse nb "succ(succ(succ(pred(succ(0)))))"
	print $ fst $ head $ parse nb "succ(succ(succ(pred(succ(0)))))"
