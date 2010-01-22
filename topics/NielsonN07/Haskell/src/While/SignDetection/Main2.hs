-- This is a variation on module While.ProgramAnalysis.Main1.
-- The sign-detection analysis is carried out with increased precision.

module While.SignDetection.Main2 where

import Prelude hiding (Ord, (<=), lookup)
import DenotationalSemantics.State
import While.AbstractSyntax (Var, Stm, factorial)
import While.DenotationalSemantics.Meanings
import While.DenotationalSemantics.Interpreter
import While.DenotationalSemantics.Values
import While.DenotationalSemantics.DirectStyle
import ProgramAnalysis.Domains
import ProgramAnalysis.TT
import ProgramAnalysis.Map (Map, maps, keys, atomic)
import qualified ProgramAnalysis.Map as Map
import ProgramAnalysis.Fix
import While.SignDetection.Sign
import While.SignDetection.Values
import While.SignDetection.Main1 hiding (analyse, main)


-- Assembly of the semantics

analyse :: Stm -> MS
analyse = fold alg
 where
  alg :: Meanings MA MB MS
  alg = ds abstractValues statesAsPOrdMaps cond fixEq
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
    print $ analyse factorial xpos
    let xany = Map.update "x" TopSign bottom
    print xany
    print $ analyse factorial xany

{-

> main
[("x",Pos)]
[("x",TopSign),("y",Pos)]
[("x",TopSign)]
[("x",TopSign),("y",Pos)]

-}
