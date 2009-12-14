module Main where

import Parsing
import FoldNB
import ParseNB
import NB


main = do
	print $ eval   $ fst $ head $ parse nb "0"
	print $ evalnb $ fst $ head $ parse nb "0"
	print $ eval   $ fst $ head $ parse nb "if true then false else if false then true else false fi fi"
	print $ evalnb $ fst $ head $ parse nb "if true then false else if false then true else false fi fi"
	print $ eval   $ fst $ head $ parse nb "if is pred(succ(0)) zero? then true else false fi"
	print $ evalnb $ fst $ head $ parse nb "if is pred(succ(0)) zero? then true else false fi"
	print $ eval   $ fst $ head $ parse nb "succ(succ(succ(pred(succ(0)))))"
	print $ evalnb $ fst $ head $ parse nb "succ(succ(succ(pred(succ(0)))))"
