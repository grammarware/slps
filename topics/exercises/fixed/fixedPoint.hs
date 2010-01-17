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
fac'' x = head (dropWhile (==Nothing) [ f'i n x | n <- [0..] ])
	where
		f g x = if x == 0 then Just 1 else maybe Nothing (Just . (x*)) (g (x-1))
		f'i 0 = const Nothing
		f'i n = f (f'i (n-1))

-- Test functions
main =
	do
		print $ fac 5 -- prints 120
		print $ fac' 5 -- ditto
		print $ fac'' 5 -- prints Just 120
