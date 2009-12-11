module Syntax where

data Statement
 = Seq Statement Statement
 | Skip
 | Assign Identifier AExpression
 | IfThenElse BExpression Statement Statement
 | While BExpression Statement

data AExpression
 = Number Int
 | Identifier String
 | Add AExpression AExpression
 | Sub AExpression AExpression
 | Mul AExpression AExpression

data BExpression
 = BTrue
 | BFalse
 | Equals AExpression AExpression
 | LessThanOrEqual AExpression AExpression
 | Not BExpression
 | And BExpression BExpression

type Identifier = String

{- 

y:=2;
x:=(y+4);
if (y = 2 ^ x = 6)
 then
  z := y
 else
  z := 1000;
while x≤10 do x := (x + 1)

-}

myWhile =
      Seq (Assign "y" (Number 2))
      (Seq (Assign "x" (Add (Identifier "y") (Number 4)))
      (Seq (IfThenElse (And (Equals (Identifier "y") (Number 2)) (Equals (Identifier "x") (Number 6)))
                         (Assign "z" (Identifier "y"))
                         (Assign "z" (Number 1000)))
      (While (LessThanOrEqual (Identifier "x") (Number 10))
             (Assign "x" (Add (Identifier "x") (Number 1))))))

mySkip = Seq Skip Skip
