-- Standard semantics of While in direct style.
-- We model as states from variables to numbers.

module While.DenotationalSemantics.Main1 where

import While.AbstractSyntax (Var, factorial)
import While.DenotationalSemantics.Meanings
import While.DenotationalSemantics.Interpreter
import While.DenotationalSemantics.Values
import While.DenotationalSemantics.State
import While.DenotationalSemantics.DirectStyle
import While.DenotationalSemantics.Fix


-- Domains for standard semantics in direct style

type N = Integer
type B = Bool
type S = Var -> N
type MA = S -> N
type MB = S -> B
type MS = S -> S


-- Assembly of the semantics

semantics :: Meanings MA MB MS
semantics = ds standardValues statesAsFunctions cond fixProperty
 where
  cond :: Cond B S
  cond mb ms1 ms2 s = if mb s then ms1 s else ms2 s

main = 
 do
    let s x = if x=="x" then 5 else undefined
    print $ stm semantics factorial s "y"

{-

> main
120

-}
