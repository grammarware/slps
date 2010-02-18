Countdown example from chapter 11 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.


> import CPUTime
> import Numeric

Expressions
-----------

> data Op                       =  Add | Sub | Mul | Div
> 
> valid                         :: Op -> Int -> Int -> Bool
> valid Add _ _                 =  True
> valid Sub x y                 =  x > y
> valid Mul _ _                 =  True
> valid Div x y                 =  x `mod` y == 0
>
> apply                         :: Op -> Int -> Int -> Int
> apply Add x y                 =  x + y
> apply Sub x y                 =  x - y
> apply Mul x y                 =  x * y
> apply Div x y                 =  x `div` y
>
> data Expr                     =  Val Int | App Op Expr Expr
>
> values                        :: Expr -> [Int]
> values (Val n)                =  [n]
> values (App _ l r)            =  values l ++ values r
>  
> eval                          :: Expr -> [Int]
> eval (Val n)                  =  [n | n > 0]
> eval (App o l r)              =  [apply o x y | x <- eval l
>                                               , y <- eval r
>                                               , valid o x y]

Combinatorial functions
-----------------------

> subs                          :: [a] -> [[a]]
> subs []                       =  [[]]
> subs (x:xs)                   =  yss ++ map (x:) yss
>                                  where yss = subs xs
>
> interleave                    :: a -> [a] -> [[a]]
> interleave x []               =  [[x]]
> interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)
> 
> perms                         :: [a] -> [[a]]
> perms []                      =  [[]]
> perms (x:xs)                  =  concat (map (interleave x) (perms xs))
>
> choices                       :: [a] -> [[a]]
> choices xs                    =  concat (map perms (subs xs))

Formalising the problem
-----------------------

> solution                      :: Expr -> [Int] -> Int -> Bool
> solution e ns n               =  elem (values e) (choices ns) && eval e == [n]

Brute force solution
--------------------

> split                         :: [a] -> [([a],[a])]
> split []                      =  []
> split [_]                     =  []
> split (x:xs)                  =  ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]
> 
> exprs                         :: [Int] -> [Expr]
> exprs []                      =  []
> exprs [n]                     =  [Val n]
> exprs ns                      =  [e | (ls,rs) <- split ns
>                                     , l       <- exprs ls
>                                     , r       <- exprs rs
>                                     , e       <- combine l r]
> 
> combine                       :: Expr -> Expr -> [Expr]
> combine l r                   =  [App o l r | o <- ops]
>  
> ops                           :: [Op]
> ops                           =  [Add,Sub,Mul,Div]
> 
> solutions                     :: [Int] -> Int -> [Expr]
> solutions ns n                =  [e | ns' <- choices ns
>                                     , e   <- exprs ns'
>                                     , eval e == [n]]

Combining generation and evaluation
-----------------------------------

> type Result                   =  (Expr,Int)
> 
> results                       :: [Int] -> [Result]
> results []                    =  []
> results [n]                   =  [(Val n,n) | n > 0]
> results ns                    =  [res | (ls,rs) <- split ns
>                                       , lx      <- results ls
>                                       , ry      <- results rs
>                                       , res     <- combine' lx ry]
> 
> combine'                      :: Result -> Result -> [Result]
> combine' (l,x) (r,y)          =  [(App o l r, apply o x y) | o <- ops
>                                                            , valid o x y]
> 
> solutions'                    :: [Int] -> Int -> [Expr]
> solutions' ns n               =  [e | ns'   <- choices ns
>                                     , (e,m) <- results ns'
>                                     , m == n]

Exploiting numeric properties
-----------------------------

> valid'                        :: Op -> Int -> Int -> Bool
> valid' Add x y                =  x <= y
> valid' Sub x y                =  x > y
> valid' Mul x y                =  x /= 1 && y /= 1 && x <= y
> valid' Div x y                =  y /= 1 && x `mod` y == 0
> 
> results'                      :: [Int] -> [Result]
> results' []                   =  []
> results' [n]                  =  [(Val n,n) | n > 0]
> results' ns                   =  [res | (ls,rs) <- split ns
>                                       , lx      <- results' ls
>                                       , ry      <- results' rs
>                                       , res     <- combine'' lx ry]
> 
> combine''                     :: Result -> Result -> [Result]
> combine'' (l,x) (r,y)         =  [(App o l r, apply o x y) | o <- ops
>                                                            , valid' o x y]
> 
> solutions''                   :: [Int] -> Int -> [Expr]
> solutions'' ns n              =  [e | ns'   <- choices ns
>                                     , (e,m) <- results' ns'
>                                     , m == n]

Interactive version for testing
-------------------------------

> instance Show Op where
>    show Add                   =  "+"
>    show Sub                   =  "-"
>    show Mul                   =  "*"
>    show Div                   =  "/"
> 
> instance Show Expr where
>    show (Val n)               =  show n
>    show (App o l r)           =  bracket l ++ show o ++ bracket r
>                                  where
>                                     bracket (Val n) = show n
>                                     bracket e       = "(" ++ show e ++ ")"
> 
> showtime                      :: Integer -> String
> showtime t                    =  showFFloat (Just 3)
>                                     (fromIntegral t / (10^12)) " seconds"
> 
> display                       :: [Expr] -> IO ()
> display es                    =  do t0 <- getCPUTime
>                                     if null es then
>                                        do t1 <- getCPUTime
>                                           putStr "\nThere are no solutions, verified in "
>                                           putStr (showtime (t1 - t0))
>                                      else
>                                        do t1 <- getCPUTime
>                                           putStr "\nOne possible solution is "
>                                           putStr (show (head es))
>                                           putStr ", found in "
>                                           putStr (showtime (t1 - t0))
>                                           putStr "\n\nPress return to continue searching..."
>                                           getLine
>                                           putStr "\n"
>                                           t2 <- getCPUTime
>                                           if null (tail es) then
>                                              putStr "There are no more solutions"
>                                            else
>                                              do sequence [print e | e <- tail es]
>                                                 putStr "\nThere were "
>                                                 putStr (show (length es))
>                                                 putStr " solutions in total, found in "
>                                                 t3 <- getCPUTime
>                                                 putStr (showtime ((t1 - t0) + (t3 - t2)))
>                                     putStr ".\n\n"
> 
> main                          :: IO ()
> main                          =  do putStrLn "\nCOUNTDOWN NUMBERS GAME SOLVER"
>                                     putStrLn "-----------------------------\n"
>                                     putStr "Enter the given numbers : "
>                                     ns <- readLn
>                                     putStr "Enter the target number : "
>                                     n  <- readLn
>                                     display (solutions'' ns n)
> 
> test765 = display (solutions [1,3,7,10,25,50] 765)

