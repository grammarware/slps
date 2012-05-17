-- This is a variation on module While.ProgramAnalysis.Main1.
-- The sign-detection analysis is carried out with increased precision.

module While.SignDetection.Main2 where

import qualified Prelude
import Prelude hiding (id, seq, (<=))
import SemanticsLib.Main
import qualified SemanticsLib.Map as Map
import While.AbstractSyntax (Var, Stm, factorial)
import While.Fold
import While.DenotationalSemantics.DirectStyle
import While.SignDetection.Main1 hiding (whileAlg, main)


-- Algebra for state transformers

strafos' :: STrafoAlg MS MB
strafos'  = strafos {
  cond = \mb ms1 ms2 s ->
           case mb s of
             TT       -> ms1 s
             FF       -> ms2 s
             TopTT    ->       ms1 (leastState TT mb s)
                         `lub` ms2 (leastState FF mb s)
             BottomTT -> bottom
}


-- Least feasible state for then/else branch

leastState :: B -> MB -> S -> S
leastState b f s
 = lubs [ s' |   
               s' <- maps (keys s)
             , s' <= s
             , b  <= f s'
             , atomic (keys s) s'
             ]


-- Assembly of the semantics

whileAlg :: WhileAlg MA MB MS
whileAlg = ds ttBooleans
              signNumbers
              statesAsPOrdMaps
              strafos'
 

main = 
 do
    let xpos = Map.update "x" Pos bottom
    print xpos
    print $ foldStm whileAlg factorial xpos
    let xany = Map.update "x" TopSign bottom
    print xany
    print $ foldStm whileAlg factorial xany

{-

> main
[("x",Pos)]
[("x",TopSign),("y",Pos)]
[("x",TopSign)]
[("x",TopSign),("y",Pos)]

-}
