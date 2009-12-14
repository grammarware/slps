Tautology checker example from section 10.4 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.


Tautology checker
-----------------

> data Prop                     =  Const Bool
>                               |  Var Char
>                               |  Not Prop
>                               |  And Prop Prop
>                               |  Imply Prop Prop
>
> type Subst                    =  Assoc Char Bool
>
> type Assoc k v                =  [(k,v)]
>
> find                          :: Eq k => k -> Assoc k v -> v
> find k t                      =  head [v | (k',v) <- t, k == k']
>
> eval                          :: Subst -> Prop -> Bool
> eval _ (Const b)              =  b
> eval s (Var x)                =  find x s
> eval s (Not p)                =  not (eval s p)
> eval s (And p q)              =  eval s p && eval s q
> eval s (Imply p q)            =  eval s p <= eval s q
>
> vars                          :: Prop -> [Char]
> vars (Const _)                =  []
> vars (Var x)                  =  [x]
> vars (Not p)                  =  vars p
> vars (And p q)                =  vars p ++ vars q
> vars (Imply p q)              =  vars p ++ vars q
>
> bools                         :: Int -> [[Bool]]
> bools 0                       =  [[]]
> bools (n+1)                   =  map (False:) bss ++ map (True:) bss
>                                  where bss = bools n
>
> rmdups                        :: Eq a => [a] -> [a]
> rmdups []                     =  []
> rmdups (x:xs)                 =  x : rmdups (filter (/= x) xs)
>
> substs                        :: Prop -> [Subst]
> substs p                      =  map (zip vs) (bools (length vs))
>                                  where vs = rmdups (vars p)
>
> isTaut                        :: Prop -> Bool
> isTaut p                      =  and [eval s p | s <- substs p]
