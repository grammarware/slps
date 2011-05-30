module Syntax where

-- Syntax of programs and expressions

type Prog = (FEnv,Expr)
type FEnv = [FDef]
type FDef = (String,([String],Expr))

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


-- Sample functions collected in one "library"

lib :: FEnv
lib
 = [ ("inc", (["x"], Binary Plus (Var "x") (Const 1))),
     ("dec", (["x"], Binary Plus (Var "x") (Const (-1)))),
     ("neg", (["x"], Binary Times (Var "x") (Const (-1)))),

     ("fac",
       ( ["x"]
       , IfZero (Var "x")
           (Const 1)
           (Binary Times
                   (Var "x")
                   (Apply "fac" [Apply "dec" [Var "x"]])))),
     ("exp",
       ( ["x","n"]
       , IfZero (Var "n")
           (Const 1)
           (Binary Times
                   (Var "x")
                   (Apply "exp" [Var "x", Apply "dec" [Var "n"]])))),

     -- A particular test case calling for memoization during PE:
     -- mod(x, y, v) = if x == 0 then v else if y - v + 1 == 0 then mod(x-1,y,0) else mod(x-1,y,v+1)
     ("mod",
       ( ["x","y","v"]
       , IfZero (Var "x")
           (Var "v")
           (IfZero (Apply "inc" [Binary Plus (Var "y") (Apply "neg" [Var "v"])])
              (Apply "mod" [Apply "dec" [Var "x"], Var "y", Apply "dec" [Const 0]])
              (Apply "mod" [Apply "dec" [Var "x"], Var "y", Apply "inc" [Var "v"]]))))]
