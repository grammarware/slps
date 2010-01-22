-- The domain of numbers

module SemanticsLib.Number (
    NumberAlg(NumberAlg)
  , from
  , add
  , mul
  , sub
  , eq
  , leq
  , standardNumbers
) where

data NumberAlg n b
   = NumberAlg {
    from  :: Integer -> n
  , add   :: n -> n -> n
  , mul   :: n -> n -> n
  , sub   :: n -> n -> n
  , eq    :: n -> n -> b
  , leq   :: n -> n -> b
}

standardNumbers :: NumberAlg Integer Bool
standardNumbers  = NumberAlg {
    from = id
  , add = (+)
  , mul = (*)
  , sub = (-)
  , eq  = (==)
  , leq = (<=)
}
