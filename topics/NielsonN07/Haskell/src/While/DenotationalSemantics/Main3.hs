-- Standard semantics of While in direct style.
-- We model states as lists of variable-number pairs.
-- We also model partiality of state transformers explicitly.
-- The chosen fixed point combinator is iteration-based.

module While.DenotationalSemantics.Main1 where

import Data.Maybe
import DenotationalSemantics.State
import DenotationalSemantics.Fix
import While.AbstractSyntax (Var, factorial)
import While.DenotationalSemantics.Meanings
import While.DenotationalSemantics.Interpreter
import While.DenotationalSemantics.Values
import While.DenotationalSemantics.DirectStyleMaybe


-- Domains for standard semantics in direct style

type N = Integer
type B = Bool
type S = [(Var,N)]
type MA = S -> N
type MB = S -> B
type MS = S -> Maybe S


-- Assembly of the semantics

semantics :: Meanings MA MB MS
semantics = ds standardValues statesAsListsOfPairs cond fixMaybe
 where
  cond :: Cond B S
  cond mb ms1 ms2 s = if mb s then ms1 s else ms2 s

main = 
 do
    let s = [("x",5)]
    print $ stm semantics factorial s

{-

> main
Just [("x",1),("y",120)]

-}
