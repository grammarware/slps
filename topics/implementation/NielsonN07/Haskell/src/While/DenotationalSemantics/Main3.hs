-- Standard semantics of While in direct style.
-- We model states as lists of variable-number pairs.
-- We also model partiality of state transformers explicitly.
-- The chosen fixed point combinator is iteration-based.

module While.DenotationalSemantics.Main3 where

import qualified Prelude
import Prelude hiding (id, seq)
import SemanticsLib.Main
import While.AbstractSyntax (Var, Stm, factorial)
import While.Fold
import While.DenotationalSemantics.DirectStyle (STrafoAlg(STrafoAlg), id, seq, cond, fix)
import While.DenotationalSemantics.DirectStyleMaybe


-- Semantic domains

type N = Integer
type B = Bool
type S = [(Var,N)]
type MA = S -> N
type MB = S -> B
type MS = S -> Maybe S


-- Algebra for state transformers

maybeSTrafos :: STrafoAlg MS MB
maybeSTrafos  = STrafoAlg {
    id   = Just
  , seq  = \st1 st2 s -> st1 s >>= st2
  , cond = \mb ms1 ms2 s -> if mb s then ms1 s else ms2 s
  , fix  = fixMaybe
}


-- Assembly of the semantics

whileAlg :: WhileAlg MA MB MS
whileAlg = ds standardBooleans 
              standardNumbers
              statesAsData
              maybeSTrafos


main = 
 do
    let s = [("x",5)]
    print $ foldStm whileAlg factorial s

{-

> main
Just [("x",1),("y",120)]

-}
