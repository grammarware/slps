-- Non-standard semantics of While in direct style.
-- The semantics provides a sign-detection analysis.
-- We model states as partially ordered maps from variables to numbers.

module While.SignDetection.Main1 where

import Prelude hiding (lookup)
import DenotationalSemantics.State
import ProgramAnalysis.Domains
import ProgramAnalysis.TT
import ProgramAnalysis.Map (Map)
import qualified ProgramAnalysis.Map as Map
import ProgramAnalysis.Fix
import While.AbstractSyntax (Var, factorial)
import While.DenotationalSemantics.Meanings
import While.DenotationalSemantics.Interpreter
import While.DenotationalSemantics.Values
import While.DenotationalSemantics.DirectStyle
import While.SignDetection.Sign
import While.SignDetection.Values


-- Domains for standard semantics in direct style

type N = Sign
type B = TT
type S = Map Var N
type MA = S -> N
type MB = S -> B
type MS = S -> S


-- States as partially ordered maps
statesAsPOrdMaps
 = State {
     lookup = Map.lookup
   , update = Map.update
   }


-- Assembly of the semantics

analysis :: Meanings MA MB MS
analysis = ds abstractValues statesAsPOrdMaps cond fixEq
 where
  cond :: Cond B S
  cond mb ms1 ms2 s
    = case mb s of
        TT       -> ms1 s
        FF       -> ms2 s
        TopTT    -> ms1 s `lub` ms2 s
        BottomTT -> bottom


main = 
 do
    let xpos = Map.update "x" Pos bottom
    print xpos
    print $ interpret analysis factorial xpos

{-

> main
[("x",Pos)]
[("x",TopSign),("y",TopSign)]

-}
