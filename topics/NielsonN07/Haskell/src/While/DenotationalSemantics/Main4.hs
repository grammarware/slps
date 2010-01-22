-- Standard semantics of While in continuation style.
-- We model states as lists of variable-number pairs.

module While.DenotationalSemantics.Main1 where

import DenotationalSemantics.State
import DenotationalSemantics.Fix
import While.AbstractSyntax (Var, factorial)
import While.DenotationalSemantics.Meanings
import While.DenotationalSemantics.Interpreter
import While.DenotationalSemantics.Values
import While.DenotationalSemantics.ContinuationStyle


-- Domains for standard semantics in direct style

type N = Integer
type B = Bool
type S = [(Var,N)]
type MA = S -> N
type MB = S -> B
type MS = ContF S


-- Assembly of the semantics

semantics :: Meanings MA MB MS
semantics = cs standardValues statesAsData cond fixProperty
 where
  cond :: Cond B S
  cond mb ms1 ms2 s = if mb s then ms1 s else ms2 s

main = 
 do
    let s = [("x",5)]
    print $ interpret semantics factorial id s

{-

> main
[("x",1),("y",120)]

-}
