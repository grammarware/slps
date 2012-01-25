-- SIMPLE Standard semantics of While in direct style.

module While.DenotationalSemantics.SimpleMain0 where

import While.SimpleAbstractSyntax
import While.DenotationalSemantics.SimpleDirectStyle

main = 
 do
    let s x = if x=="x" then 5 else undefined
    print $ execute factorial s "y"

{-

> main
120

-}
