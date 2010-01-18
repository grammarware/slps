module Factorial where

import Maybe

---- Assignment
-- Explicitly recursive functorial function
fac x = if x == 0 then 1 else x * fac (x-1)

-- Recursion by means of fixed point combinator
fac' = fix f
	where
		-- Functional for factorial
		f g x = if x == 0 then 1 else x * g (x-1)
		-- Fixed point combinator based on fixed point condition
		fix f = f (fix f)

-- Fixed point computation based on iteration
fac'' x = fromJust $ head $ dropWhile (==Nothing) [ f'i n x | n <- [0..] ]
	where
		f g x = if x == 0 then Just 1 else maybe Nothing (Just . (x*)) (g (x-1))
		f'i 0 = const Nothing
		f'i n = f (f'i (n-1))

-- Test functions
main =
	do
		print $ fac 5 -- all variations print 120
		print $ fac' 5
		print $ fac'' 5

---- Solution
-- 1. Does fac’’ have any “pragmatic” advantage over fac’?
-- Not in this simple case. Possibly more control over fixed point computation.

-- 2. Why is it a good idea to use the Maybe constructor in fac''.
-- Becase "no answer" is an ideologically correct & anticipated answer.

-- 3. Why could we start from 1 rather than from 0 in fac''?
-- Apparent when we inline the head.

-- 4. How could fac’’ be optimized while still performing the same iteration?
-- The first way is to move the lower bound up:
fac''op1 x = fromJust $ head $ dropWhile (==Nothing) [ f'i n x | n <- [1..] ]
	where
		f g x = if x == 0 then Just 1 else maybe Nothing (Just . (x*)) (g (x-1))
		f'i 0 = const Nothing
		f'i n = f (f'i (n-1))
-- Or even farther:
fac''op2 x = fromJust $ head $ dropWhile (==Nothing) [ f'i n x | n <- [x..] ]
	where
		f g x = if x == 0 then Just 1 else maybe Nothing (Just . (x*)) (g (x-1))
		f'i 0 = const Nothing
		f'i n = f (f'i (n-1))
-- Starting from x+1, we don’t need dropWhile
fac''op3 x = fromJust $ head [ f'i n x | n <- [x+1..] ]
	where
		f g x = if x == 0 then Just 1 else maybe Nothing (Just . (x*)) (g (x-1))
		f'i 0 = const Nothing
		f'i n = f (f'i (n-1))
-- The second way is to move the upper bound:
fac''op4 x = fromJust $ head $ dropWhile (==Nothing) [ f'i n x | n <- [0..x+1] ]
	where
		f g x = if x == 0 then Just 1 else maybe Nothing (Just . (x*)) (g (x-1))
		f'i 0 = const Nothing
		f'i n = f (f'i (n-1))
-- Ultimately (incorrect assignment answer!), we can reduce the list to one element
--  fac''op5 x = fromJust $ head $ dropWhile (==Nothing) [ f'i (x+1) x ]
-- and then get rid of dropWhile:
--  fac''op5 x = fromJust $ head [ f'i (x+1) x ]
-- and head:
fac''op5 x = fromJust $ f'i (x+1) x
	where
		f g x = if x == 0 then Just 1 else maybe Nothing (Just . (x*)) (g (x-1))
		f'i 0 = const Nothing
		f'i n = f (f'i (n-1))
-- The third good optimisation would be to make it use the previously computed factorials:
-- Remember the straightforward definition of the factorial:
--  fac x = product [1..x]
-- which is, as we know:
--  fac x = foldl (*) 1 [1..x]
-- which in turn can be implemented as:
--  fac''' x = (foldl (\ f n -> f . (n*)) id [1..x]) 1
-- Let's reproduce it.
fac''' x = head $ dropWhile (==0) (map f [0..])
	where
		f l = fold l (\ f n -> f . (n*)) id [1..x] 1
		fold 0 _ _ _ = const 0
		fold _ _ z [] = z
		fold n f z (x:xs) = fold (n-1) f (f z x) xs
-- Indeed, if we compute all factorials of numbers from 1 to 2000
--  with fac'' it takes 8 seconds,
--  with fac''' it takes 3 seconds

-- 5. Would it be safe to take the second element in fac'' rather than head?
-- Yes. First, second and all elements after that are equal.
