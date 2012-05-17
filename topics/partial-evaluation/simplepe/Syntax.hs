module Syntax where

import Prelude hiding (mod)

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
 = [ ("neg", (["x"], Binary Times (Var "x") (Const (-1)))),
     ("add", (["x", "y"], Binary Plus (Var "x") (Var "y"))),
     ("minus", (["x", "y"], Binary Plus (Var "x") (Apply "neg" [Var "y"]))),
     ("times", (["x", "y"], Binary Times (Var "x") (Var "y"))),
     ("inc", (["x"], Binary Plus (Var "x") (Const 1))),
     ("dec", (["x"], Binary Plus (Var "x") (Const (-1)))),

     ("fac",
       ( ["x"]
       , IfZero (Var "x")
           (Const 1)
           (Apply "times" [Var "x", Apply "fac" [Apply "dec" [Var "x"]]]))),

     ("exp",
       ( ["x","n"]
       , IfZero (Var "n")
           (Const 1)
           (Apply "times" [Var "x", Apply "exp" [Var "x", Apply "dec" [Var "n"]]]))),

     -- A particular test case calling for memoization during PE:
     ("mod", (["x","y"], Apply "mod3" [Var "x", Var "y", Const 0])),
     ("mod3",
       ( ["x","y","v"]
       , IfZero (Var "x")
           (Var "v")
           (Apply "mod3" [
             Apply "dec" [Var "x"],
             Var "y",
             IfZero (Apply "minus" [Var "y", Apply "inc" [Var "v"]])
               (Const 0)
               (Apply "inc" [Var "v"])])))]

-- Haskell version of the above code

mod x y = mod3 x y 0
mod3 x y v
 = if x == 0
     then v
     else mod3 (x-1) y (if y - (v + 1) == 0 then 0 else v+1)
