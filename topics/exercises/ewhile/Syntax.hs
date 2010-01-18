module Syntax where

data Statement
	= Seq Statement Statement
	| Skip
	| Assign Identifier AExpression
	| IfThenElse BExpression Statement Statement
	| While BExpression Statement
	| Block [VariableDeclaration] [ProcedureDeclaration] Statement
	| Call Procedure

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

data VariableDeclaration = Var Identifier AExpression

data ProcedureDeclaration = Proc Procedure Statement

type Procedure = String
type Identifier = String

myWhile =
      Seq (Assign "y" (Number 2))
      (Seq (Assign "x" (Add (Identifier "y") (Number 4)))
      (Seq (IfThenElse (And (Equals (Identifier "y") (Number 2)) (Equals (Identifier "x") (Number 6)))
                         (Assign "z" (Identifier "y"))
                         (Assign "z" (Number 1000)))
      (While (LessThanOrEqual (Identifier "x") (Number 10))
             (Assign "x" (Add (Identifier "x") (Number 1))))))

mySkip = Seq Skip Skip

myBlock = Block
				[Var "x" (Number 1)]
				[]
				(While (LessThanOrEqual (Identifier "x") (Number 10))
					(Block
						[Var "y" (Add (Identifier "x") (Number 1))]
						[Proc "inc" (Assign "x" (Identifier "y"))]
						(Call "inc")))
