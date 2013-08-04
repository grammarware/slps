module Parser where

import AbstractSyntax
import Parsing

nb = true +++ false +++ cond +++ zero +++ succnb +++ prednb +++ iszero

true = 
 do
    token (string "true")
    return TrueB

false = 
 do
    token (string "false")
    return FalseB

cond = 
 do
    token (string "if")
    x <- nb
    token (string "then")
    y <- nb
    token (string "else")
    z <- nb
    token (string "fi")
    return (IfNB x y z)

zero = 
 do
    token (string "0")
    return ZeroN

succnb = 
 do
    token (string "succ")
    token (string "(")
    x <- nb
    token (string ")")
    return (SuccN x)

prednb = 
 do
    token (string "pred")
    token (string "(")
    x <- nb
    token (string ")")
    return (PredN x)

iszero = 
 do
    token (string "is")
    x <- nb
    token (string "zero?")
    return (IsZeroB x)
