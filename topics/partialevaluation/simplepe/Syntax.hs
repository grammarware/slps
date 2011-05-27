module Syntax where

-- Syntax of programs and expressions

type Prog = (FEnv,Expr)
type FEnv = [(String,([String],Expr))]

data Expr
 = Const Int
 | Var String
 | Binary Op Expr Expr
 | IfZero Expr Expr Expr
 | Apply String [Expr]
 deriving (Show)

data Op = Plus | Times
 deriving (Show)

op2f :: Op -> (Int -> Int -> Int)
op2f Plus = (+) 
op2f Times = (*)


-- Sample functions collected in one "library"

lib :: FEnv
lib
 = [ ("fac",
       ( ["x"]
       , IfZero (Var "x")
           (Const 1)
           (Binary Times
                   (Var "x")
                   (Apply "fac" [(Binary Plus
                                         (Var "x")
                                         (Const (-1)))])))),
     ("exp",
       ( ["x","n"]
       , IfZero (Var "n")
           (Const 1)
           (Binary Times
                   (Var "x")
                   (Apply "exp" [Var "x",(Binary Plus
                                                 (Var "n")
                                                 (Const (-1)))])))) ]
