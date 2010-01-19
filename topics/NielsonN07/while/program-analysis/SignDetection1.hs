-- Sign-detection analysis

module SignDetection1 where

import qualified Prelude
import Prelude hiding (not, and, (&&), lookup, id)
import WhileSyntax
import WhileDomains
import WhileDirectStyle
import WhileInterpreter
import Ordering
import qualified TT
import TT hiding (not)
import Sign
import qualified Map
import Map (Map)
import Fix


-- Domains for the program analysis

type N = Sign
type B = TT
type S = Map Var N
type A = S -> N
type L = S -> B
type T = S -> S


domains :: Domains N B S A L T
domains = Domains {

  -- Numbers
    num = fromInteger
  , add = (+)
  , mul = (*)
  , sub = (-)
  , eq  = (.==.)
  , leq = (.<=.)

  -- Booleans 
  , true  = TT
  , false = FF
  , not   = TT.not
  , and   = (&&)

  -- States
  , lookup = Map.lookup
  , update = Map.update

  -- State transformers
  , mk   = Prelude.id
  , id   = Prelude.id
  , o    = (.)
  , cond = \b t1 t2 s ->
             case b s of
               TT       -> t1 s
               FF       -> t2 s
               TopTT    -> t1 s `lub` t2 s
               BottomTT -> bottom
  , fix  = fixEq
 
}


-- Time for testing

analysis = stm (ds domains)

main = 
 do
    let xpos = Map.update "x" Pos bottom
    print $ xpos
    print $ analysis factorial xpos

{-

> let xpos = Map.update "x" Pos bottom
> xpos
[("x",Pos)]
> analysis factorial xpos
[("x",TopSign),("y",TopSign)]

-}
