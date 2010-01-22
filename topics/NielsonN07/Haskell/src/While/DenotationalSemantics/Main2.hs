-- Standard semantics of While in direct style.
-- We model states as lists of variable-number pairs.

module While.DenotationalSemantics.Main2 where

import qualified Prelude
import Prelude hiding (id, seq)
import SemanticsLib.Main
import While.AbstractSyntax (Var, Stm, factorial)
import While.Fold
import While.DenotationalSemantics.DirectStyle
import While.DenotationalSemantics.Main1 (standardSTrafos)


-- Semantic domains

type N = Integer
type B = Bool
type S = [(Var,N)]
type MA = S -> N
type MB = S -> B
type MS = S -> S


-- Assembly of the semantics

whileAlg :: WhileAlg MA MB MS
whileAlg = ds standardBooleans
              standardNumbers
              statesAsData
              standardSTrafos


main = 
 do
    let s = [("x",5)]
    print $ foldStm whileAlg factorial s

{-

> main
[("x",1),("y",120)]

-}
