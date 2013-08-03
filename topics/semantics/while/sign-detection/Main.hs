module Main (
    module SemanticsLib.Boolean
  , module SemanticsLib.Number
  , module SemanticsLib.State
  , module SemanticsLib.Domain
  , module SemanticsLib.TT
  , module SemanticsLib.Map
  , module Sign
) where

import qualified Prelude
import Prelude hiding (id, seq, (<=))
import qualified SemanticsLib.Map as Map
import While.AbstractSyntax 
import While.Fold
import While.DenotationalSemantics.DirectStyle
import SemanticsLib.Boolean
import SemanticsLib.Number
import SemanticsLib.State
import SemanticsLib.Domain
import SemanticsLib.TT
import SemanticsLib.Map hiding (lookup, update)
import Sign
import Data.Maybe

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
             TopTT    ->       ms1 (leastState TT mb s)
                         `lub` ms2 (leastState FF mb s)
             BottomTT -> bottom
 , fix  = (\s -> fromMaybe (error "Divergence detected!") . fixEq s)
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
              strafos


-- Assignment 9 solution

fixEq :: (Bottom x, Eq x, POrd x)
      => ((x -> x) -> x -> x) -> x -> Maybe x

fixEq f x = iterate (const bottom)
 where 
  iterate r = let r' = f r  in
               if r x == r' x
                 then Just (r x)
                 else if r x <= r' x
                      then iterate r'
                      else Nothing

testOne
 = Seq
     (Assign "x" (Num 1))
     (Seq
       (Assign "y-1" (Sub (Var "x") (Num 1)))
       (Seq
         (Assign "x+1" (Add (Var "x") (Num 1)))
         (Assign "(x+1)-1" (Sub (Var "x+1") (Num 1)))
       )
     )

testIf
 = Seq
     (Assign "x" (Num 5))
     (Seq
        (Assign "y" (Num 10))
        (If
           (Eq (Var "x") (Var "y"))
           (Assign "z" (Num 1))
           (Assign "z" (Num 0)))
     )

main = 
 do
    let xpos = Map.update "x" Zero bottom
    print xpos
    print $ foldStm whileAlg factorial xpos
    print $ foldStm whileAlg testOne xpos
    print $ foldStm whileAlg testIf xpos
