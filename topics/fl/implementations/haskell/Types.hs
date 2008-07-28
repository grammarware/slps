{-# OPTIONS -fglasgow-exts #-}

module Types where

import Data.Generics

data Function = Function Name [Name] Expr
     deriving (Eq,Show,Typeable,Data)
type Name = String
data Expr = 
       Literal Int
     | Argument Name
     | Binary Ops Expr Expr
     | IfThenElse Expr Expr Expr 
     | Apply Name [Expr]
     deriving (Eq,Show,Typeable,Data)
data Ops= Equal | Plus | Minus
     deriving (Eq,Show,Typeable,Data)
