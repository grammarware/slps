module While.Fold where

import Prelude hiding (Num, True, False)
import While.AbstractSyntax


-- A fold algebra for While

data WhileAlg 
      ra -- Results for arithmetic expressions
      rb -- Results for Boolean expressions
      rs -- Results for statements
   = WhileAlg {

    numF :: Num -> ra
  , varF :: Var -> ra
  , addF :: ra -> ra -> ra
  , mulF :: ra -> ra -> ra
  , subF :: ra -> ra -> ra
  
  , trueF  :: rb
  , falseF :: rb
  , eqF    :: ra -> ra -> rb
  , leqF   :: ra -> ra -> rb
  , notF   :: rb -> rb
  , andF   :: rb -> rb -> rb

  , assignF :: Var -> ra -> rs
  , skipF   :: rs
  , seqF    :: rs -> rs -> rs
  , ifF     :: rb -> rs -> rs -> rs
  , whileF  :: rb -> rs -> rs
}

foldAexp :: WhileAlg ra rb rs -> Aexp -> ra
foldBexp :: WhileAlg ra rb rs -> Bexp -> rb
foldStm  :: WhileAlg ra rb rs -> Stm -> rs

foldAexp f (Num n)      = numF f n
foldAexp f (Var x)      = varF f x
foldAexp f (Add a1 a2)  = addF f (foldAexp f a1) (foldAexp f a2)
foldAexp f (Mul a1 a2)  = mulF f (foldAexp f a1) (foldAexp f a2)
foldAexp f (Sub a1 a2)  = subF f (foldAexp f a1) (foldAexp f a2)

foldBexp f True        = trueF f
foldBexp f False       = falseF f
foldBexp f (Eq a1 a2)  = eqF f (foldAexp f a1) (foldAexp f a2)
foldBexp f (Leq a1 a2) = leqF f (foldAexp f a1) (foldAexp f a2)
foldBexp f (Not b1)    = notF f (foldBexp f b1)
foldBexp f (And b1 b2) = andF f (foldBexp f b1) (foldBexp f b2)

foldStm f (Assign x a)  = assignF f x (foldAexp f a)
foldStm f Skip          = skipF f
foldStm f (Seq s1 s2)   = seqF f (foldStm f s1) (foldStm f s2)
foldStm f (If b1 s1 s2) = ifF f (foldBexp f b1) (foldStm f s1) (foldStm f s2)
foldStm f (While b s)   = whileF f (foldBexp f b) (foldStm f s)
