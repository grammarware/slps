module B where

-- see Recursive Types, slide 420 and further
data B
    = TrueB
    | FalseB
    | IfB B B B
    deriving Show

