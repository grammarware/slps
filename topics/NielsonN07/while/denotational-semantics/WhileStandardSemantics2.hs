-- Variation on standard semantics of While in direct style

module WhileStandardSemantics2 where

import qualified Prelude
import Prelude hiding (id, lookup)
import WhileSyntax (Var, factorial)
import WhileDomains
import WhileDirectStyle
import WhileInterpreter
import WhileStandardSemantics1 hiding (S, A, L, T, main)
import Fix


-- A variation on the standard semantics:
-- * States are represented as lists of pairs.
-- * State transformers are explicitly partial.
-- * Fixed points are determined by means of iteration.

-- Domains

type S = [(Var,N)]
type A = S -> N
type L = S -> B
type T = S -> Maybe S

domains' :: Domains N B S A L T
domains' = domains {

  -- States

    lookup = let lookup x ((x',n):s)
                  = if (x==x') then n else lookup x s
             in lookup

  , update = let update x n [] = [(x,n)]
                 update x n ((x',n'):s)
                  = if (x==x')
                      then ((x',n):s)
                      else ((x',n'):update x n s)
             in update

  -- State transformers
  , mk   = (.) Just
  , id   = Just
  , o    = \t1 t2 s   -> maybe Nothing t1 (t2 s)
  , cond = \b t1 t2 s -> if b s then t1 s else t2 s
  , fix  = fixMaybe

}


-- Time for testing

semantics' = stm (ds domains')

main = 
 do
    let s = [("x",5)]
    print $ semantics' factorial s

{-

> main
Just [("x",1),("y",120)]

-}
