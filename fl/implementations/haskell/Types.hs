{-# OPTIONS -fglasgow-exts #-}

module Types where

import Data.Generics

data Function = Function Head Expr
     deriving (Eq,Show,Typeable,Data)
type Head = (Name,Formals)
type Name = String
type Formals = [Name]
data Expr = 
       Literal Int
     | Argument Name
     | Binary Op Expr Expr
     | IfThenElse Expr Expr Expr 
     | Apply Name Actuals
     deriving (Eq,Show,Typeable,Data)
type Actuals = [Expr]
data Op = Equal | Plus | Minus
     deriving (Eq,Show,Typeable,Data)
