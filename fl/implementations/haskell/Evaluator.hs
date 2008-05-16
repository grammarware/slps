module Evaluator where

import Prelude hiding (lookup)
import Data.Map
import Types

evaluate :: [Function] -> Expr -> Maybe Int
evaluate fs e = evaluate empty e
  where
    evaluate m (Literal i) = Just i
    evaluate m (Argument n) = lookup n m
    evaluate m (Binary o x y) = do 
    	     x' <- evaluate m x
	     y' <- evaluate m y
             return (case o of
               Equal -> if x'==y' then -1 else 0
               Plus -> x' + y'
               Minus -> x' - y') 
    evaluate m (IfThenElse x y z) = do 
    	     x' <- evaluate m x
	     if x' /= 0 then evaluate m y else evaluate m z
    evaluate m (Apply n es) = do
    	     is <- mapM (evaluate m) es
    	     (ns,e) <- head ([ Just (ns,e) | (Function (n',ns) e) <- fs, n == n' ] ++ [Nothing])
	     let m' = fromList (zip ns is)
	     evaluate m' e
