-- Variation on sign-detection analysis

module SignDetection2 where

import WhileSyntax
import WhileDomains
import WhileDirectStyle
import WhileInterpreter
import Ordering
import TT
import Sign
import qualified Map
import Map (maps, keys, atomic)
import SignDetection1 hiding (main)


-- Obtain feasible, atomic states from condition

states :: B -> (S -> B) -> S -> [S]
states b f s = [ s' |   s' <- maps (keys s)
                      , s' <= s
                      , b  <= f s'
                      , atomic (keys s) s'
               ]


-- The refined handling of conditionals

domains' :: Domains N B S A L T
domains' = domains {

    cond = \b t1 t2 s ->
             case b s of
               TT       -> t1 s
               FF       -> t2 s
               TopTT    ->       t1 (lubs (states TT b s))
                           `lub` t2 (lubs (states FF b s))
               BottomTT -> bottom

}


-- The refined program analysis

analysis' = stm (ds domains')

main = 
 do
    let xany = Map.update "x" TopSign bottom
    print $ xany
    print $ analysis' factorial xany
    let xpos = Map.update "x" Pos bottom
    print $ xpos
    print $ analysis' factorial xpos

{-

> let xany = Map.update "x" TopSign bottom
> xany
[("x",TopSign)]
> analysis' factorial xpos
[("x",TopSign),("y",Pos)]
> let xpos = Map.update "x" Pos bottom
> xpos
> analysis' factorial xpos
[("x",TopSign),("y",Pos)]

-}
