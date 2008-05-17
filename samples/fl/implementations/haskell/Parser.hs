module Parser where

import Text.ParserCombinators.ReadP
import Types
import Scanner


-- Parse from file

parseFile f = do
	s <- readFile f
	let r = readP_to_S program s
	if (length r == 1)
	   then return (fst (head r))
	   else fail "parse error"


-- Programs as lists of function definitions

program = do
	fs <- many1 function
	eof
	return fs


-- Function definitions

function = do
	 n <- name
	 ns <- many name
	 special "="
	 e <- expr
	 many1 eoln
	 return (Function (n,ns) e)


-- All expression forms

expr = literal
       	 +++ argument
      	 +++ binary
      	 +++ ifThenElse
      	 +++ apply


-- Literals (unsigned integer constants)

literal = do is <- int; return (Literal (read is))


-- Reference to function arguments

argument = do n <- name; return (Argument n)


-- Binary expression form

binary = inParens (
       do 
       	  e1 <- expr
	  o <- ops
       	  e2 <- expr
       	  return (Binary o e1 e2))

ops =
     (special "==" >> return Equal)
 +++ (special "+" >> return Plus)
 +++ (special "-" >> return Minus)


-- Conditionals

ifThenElse = do
	   keyword "if"
	   e1 <- expr
	   keyword "then"
	   e2 <- expr
	   keyword "else"
	   e3 <- expr
	   return (IfThenElse e1 e2 e3)


-- Function applications

apply = inParens (
      do
            n <- name
      	    es <- many expr
	    return (Apply n es))


-- Some construct in parentheses

inParens p = do 
       special "("
       r <- p
       special ")"
       return r
