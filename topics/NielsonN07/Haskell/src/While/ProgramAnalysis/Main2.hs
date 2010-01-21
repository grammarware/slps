-- This is a variation on module While.ProgramAnalysis.Main1.
-- The sign-detection analysis is carried out with increased precision.

module While.ProgramAnalysis.Main2 where

import Prelude hiding (Ord, (<=), lookup)
import While.AbstractSyntax (Var, factorial)
import While.DenotationalSemantics.Meanings
import While.DenotationalSemantics.Interpreter
import While.DenotationalSemantics.Values
import While.DenotationalSemantics.State
import While.DenotationalSemantics.DirectStyle
import While.ProgramAnalysis.Domains
import While.ProgramAnalysis.TT
import While.ProgramAnalysis.Sign
import While.ProgramAnalysis.Map (Map, maps, keys, atomic)
import qualified While.ProgramAnalysis.Map as Map
import While.ProgramAnalysis.Values
import While.ProgramAnalysis.Fix
import While.ProgramAnalysis.Main1 hiding (analysis, main)


-- Assembly of the semantics

analysis :: Meanings MA MB MS
analysis = ds abstractValues statesAsPOrdMaps cond fixEq
 where
  cond mb ms1 ms2 s
    = case mb s of
        TT       -> ms1 s
        FF       -> ms2 s
        TopTT    ->       ms1 (lubs (feasibleStates TT mb s))
                    `lub` ms2 (lubs (feasibleStates FF mb s))
        BottomTT -> bottom


-- Obtain feasible states

feasibleStates :: B -> MB -> S -> [S]
feasibleStates b f s = [ s' |   
                              s' <- maps (keys s)
                            , s' <= s
                            , b  <= f s'
                            , atomic (keys s) s'
                       ]


main = 
 do
    let xpos = Map.update "x" Pos bottom
    print xpos
    print $ stm analysis factorial xpos
    let xany = Map.update "x" TopSign bottom
    print xany
    print $ stm analysis factorial xany

{-

> main
[("x",Pos)]
[("x",TopSign),("y",Pos)]
[("x",TopSign)]
[("x",TopSign),("y",Pos)]

-}
