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

data Op = Plus | Times | Mod
 deriving (Show)

op2f :: Op -> (Int -> Int -> Int)
op2f Plus = (+) 
op2f Times = (*)
op2f Mod = mod


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
                                                 (Const (-1)))])))),

     -- A particular test case calling for memoization during PE:
     -- test(x, n, v) = if v == 0 then v else test(x + 1 mod n, n, v - 1)
     ("test",
       ( ["x","n","v"]
       , IfZero (Var "v")
           (Var "v")
           (Apply "test" [Binary Mod (Binary Plus (Var "x") (Const 1)) (Var "n"),
                          Var "n",
                          Binary Plus (Var "v") (Const (-1))])))]
