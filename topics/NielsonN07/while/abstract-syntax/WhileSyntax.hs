module WhileSyntax where

import Prelude hiding (Num, True, False)


-- Numbers

type {- n <- -} Num = Integer


-- Variables

type {- x <- -} Var = String 


-- Arithmetic expressions

data {- a <- -} Aexp
 = Num Num
 | Var Var
 | Add Aexp Aexp
 | Mul Aexp Aexp
 | Sub Aexp Aexp
 deriving (Show)


-- Boolean expressions

data {- b <- -} Bexp
 = True
 | False
 | Eq Aexp Aexp
 | Leq Aexp Aexp
 | Not Bexp
 | And Bexp Bexp
 deriving (Show)


-- Statements

data {- s <- -} Stm
 = Assign Var Aexp
 | Skip
 | Seq Stm Stm
 | IfElse Bexp Stm Stm
 | While Bexp Stm
 deriving (Show)

{- 

y = 1;
while (!(x<=1)) do {
  y = y * x;
  x = x - 1;
}

-}

factorial
 = Seq (Assign "y" (Num 1))
       (While (Not (Leq (Var "x") (Num 1)))
              (Seq (Assign "y" (Mul (Var "y") (Var "x")))
                   (Assign "x" (Sub (Var "x") (Num 1)))))
