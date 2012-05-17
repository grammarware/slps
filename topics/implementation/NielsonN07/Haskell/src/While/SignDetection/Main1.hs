-- Non-standard semantics of While in direct style.
-- The semantics provides a sign-detection analysis.
-- We model states as partially ordered maps from variables to numbers.

module While.SignDetection.Main1 where

import qualified Prelude
import Prelude hiding (id, seq)
import SemanticsLib.Main
import qualified SemanticsLib.Map as Map
import While.AbstractSyntax (Var, Stm, factorial)
import While.Fold
import While.DenotationalSemantics.DirectStyle


-- Semantic domains for analysis

type N = Sign
type B = TT
type S = Map Var N
type MA = S -> N
type MB = S -> B
type MS = S -> S


-- Algebra for state transformers

strafos :: STrafoAlg MS MB
strafos  = STrafoAlg {
    id   = Prelude.id
  , seq  = flip (.)
  , cond = \mb ms1 ms2 s ->
             case mb s of
               TT       -> ms1 s
               FF       -> ms2 s
               TopTT    -> ms1 s `lub` ms2 s
               BottomTT -> bottom
  , fix  = fixEq2
}


-- Assembly of the semantics

whileAlg :: WhileAlg MA MB MS
whileAlg = ds ttBooleans
              signNumbers
              statesAsPOrdMaps
              strafos


main = 
 do
    let xpos = Map.update "x" Pos bottom
    print xpos
    print $ foldStm whileAlg factorial xpos

{-

> main
[("x",Pos)]
[("x",TopSign),("y",TopSign)]

-}
