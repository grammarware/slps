-- Standard semantics of While in continuation style.
-- We model states as lists of variable-number pairs.

module While.DenotationalSemantics.Main4 where


import qualified Prelude
import Prelude hiding (id, seq)
import SemanticsLib.Main
import While.AbstractSyntax (Var, Stm, factorial)
import While.Fold
import While.DenotationalSemantics.ContinuationStyle


-- Semantic domains

type N = Integer
type B = Bool
type S = [(Var,N)]
type MA = S -> N
type MB = S -> B
type MS = ContT S


-- Algebra for continuation transformers

ctrafos  = CTrafoAlg {
    id   = Prelude.id
  , seq  = (.)
  , cond = \mb ms1 ms2 c s -> if mb s then ms1 c s else ms2 c s
  , fix  = fixProperty
}


-- Assembly of the semantics

whileAlg :: WhileAlg MA MB MS
whileAlg = cs standardBooleans
              standardNumbers
              statesAsData
              ctrafos


main = 
 do
    let s = [("x",5)]
    print $ foldStm whileAlg factorial (Prelude.id) s

{-

> main
[("x",1),("y",120)]

-}
