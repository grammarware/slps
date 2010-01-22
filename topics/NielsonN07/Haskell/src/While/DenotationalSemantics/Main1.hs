-- Standard semantics of While in direct style.
-- We model as states from variables to numbers.

module While.DenotationalSemantics.Main1 where

import qualified Prelude
import Prelude hiding (id, seq)
import SemanticsLib.Main
import While.AbstractSyntax (Var, Stm, factorial)
import While.Fold
import While.DenotationalSemantics.DirectStyle


-- Domains for standard semantics in direct style

type N = Integer
type B = Bool
type S = Var -> N
type MA = S -> N
type MB = S -> B
type MS = S -> S


-- Polymorphic algebra for state transformers

strafos  = STrafoAlg {
    id   = Prelude.id
  , seq  = flip (.)
  , cond = \mb ms1 ms2 s -> if mb s then ms1 s else ms2 s
  , fix  = fixProperty
}


-- Assembly of the semantics

execute :: Stm -> MS
execute = foldStm alg 
 where 
  alg :: WhileAlg MA MB MS
  alg = ds standardBooleans standardNumbers statesAsFunctions strafos


main = 
 do
    let s x = if x=="x" then 5 else undefined
    print $ execute factorial s "y"

{-

> main
120

-}
