-- Standard semantics of While in direct style

module WhileStandardSemantics1 where

import qualified Prelude
import Prelude hiding (not, and, lookup, id)
import WhileSyntax (Var, factorial)
import WhileDomains
import WhileDirectStyle
import WhileInterpreter
import Fix


-- Domains for standard semantics

type N = Integer
type B = Bool
type S = Var -> N
type A = S -> N
type L = S -> B
type T = S -> S

domains :: Domains N B S A L T
domains = Domains {

  -- Numbers
    num = Prelude.id
  , add = (+)
  , mul = (*)
  , sub = (-)
  , eq  = (==)
  , leq = (<=)

  -- Booleans 
  , true  = Prelude.True
  , false = Prelude.False
  , not   = Prelude.not
  , and   = (&&)

  -- States
  , lookup = flip ($)
  , update = \x n s x' -> if (x==x') then n else s x'

  -- State transformers
  , mk   = Prelude.id
  , id   = Prelude.id
  , o    = (.)
  , cond = \b t1 t2 s -> if b s then t1 s else t2 s
  , fix  = fixRecursive
 
}


-- Time for testing

semantics = stm (ds domains)

main = 
 do
    let s x = if x=="x" then 5 else undefined
    print $ semantics factorial s "y"

{-

> main
120

-}
