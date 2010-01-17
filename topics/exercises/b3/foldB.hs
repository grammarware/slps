import B

-- see slides 426-429 for the fold for arithmetic expressions
foldB :: r -> r -> (r -> r -> r -> r) -> B -> r
foldB r _ _ TrueB = r
foldB _ r _ FalseB = r
foldB r1 r2 f (IfB x y z) =
	f (fold x) (fold y) (fold z)
	where fold = foldB r1 r2 f

depth :: B -> Int
depth = foldB
	1
	1
	(\ x y z -> 1 + maximum [x, y, z])

countT :: B -> Int
countT = foldB
	1
	0
	(\ x y z -> x + y + z)

countF :: B -> Int
countF = foldB
	0
	1
	(\ x y z -> x + y + z)

eval :: B -> Bool
eval = foldB
	True
	False
	(\ x y z -> if x then y else z)

main = 
	print (eval (IfB (IfB TrueB FalseB FalseB) FalseB FalseB))

