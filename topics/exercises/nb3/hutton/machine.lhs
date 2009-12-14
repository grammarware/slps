Abstract machine example from section 10.5 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.


Abstract machine
----------------

> data Expr                     =  Val Int | Add Expr Expr
> 
> type Cont                     =  [Op]
>
> data Op                       =  EVAL Expr | ADD Int
> 
> eval                          :: Expr -> Cont -> Int
> eval (Val n)   c              =  exec c n
> eval (Add x y) c              =  eval x (EVAL y : c)
> 
> exec                          :: Cont -> Int -> Int
> exec []             n         =  n
> exec (EVAL y : c) n           =  eval y (ADD n : c)
> exec (ADD n  : c) m           =  exec c (n+m)
> 
> value                         :: Expr -> Int
> value e                       =  eval e []
