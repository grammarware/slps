module Parser where

import Types
import Scanner
import ParserLib


-- Programs as lists of function definitions

program = many1 function


-- Function definitions

function = do
	 n <- name
	 ns <- many name
	 special "="
	 e <- expr
	 many1 eoln
	 return (Function n ns e)


-- Top-level layer of expression forms

expr =  binary +++ apply +++ ifThenElse


-- Layer of binary expressions

binary = lassoc ops atom Binary
  where
    -- Operation symbols
    ops =
    	(special "==" >> return Equal)
	+++ (special "+" >> return Plus)
	+++ (special "-" >> return Minus)


-- Function applications

apply =
      do
            n <- name
      	    es <- many atom
	    return (Apply n es)


-- Conditionals

ifThenElse = do
	   keyword "if"
	   e1 <- expr
	   keyword "then"
	   e2 <- expr
	   keyword "else"
	   e3 <- expr
	   return (IfThenElse e1 e2 e3)


-- Final layer of expression forms

atom = literal +++ argument +++ inParens


-- Literals (integer constants)

literal = do i <- int; return (Literal i)


-- Reference to function arguments

argument = do n <- name; return (Argument n)


-- Expression in parentheses

inParens = do 
       special "("
       e <- expr
       special ")"
       return e
