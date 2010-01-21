module While.ProgramAnalysis.Fix where

import While.DenotationalSemantics.Fix
import While.ProgramAnalysis.Domains


-- Equality-based fixed-point combinator

fixEq :: (Bottom x, Eq x)
      => ((x -> x) -> x -> x) -> x -> x

fixEq f x = findEq (repetitions bottom f x)
 where
  findEq (h:h':t)
   = if h==h'
       then h
       else findEq (h':t)
