---- Exercises block 1, slides 285–286, (3), (4)
-- take an array, return the last element
last1 :: [a] -> a
last1 (x:xs) = if null xs then x else last1 xs

last2 :: [a] -> a
last2 = head . reverse

last3 :: [a] -> a
last3 xs = head (drop (length xs - 1) xs)

last4 :: [a] -> a
last4 xs = xs !! (length xs - 1)

---- Exercises block 1, slides 285–286, (5)
-- take an array, remove the last element
init1 :: [a] -> [a]
init1 = reverse . tail . reverse

init2 :: [a] -> [a]
init2 = reverse . (drop 1) . reverse

init3 :: [a] -> [a]
init3 xs = take (length xs - 1) xs

init4 :: [a] -> [a]
init4 xs = if length xs == 1 then [] else [head xs] ++ init4 (tail xs)

---- Exercises block 3, slides 315–316, (2)
-- take an array, return True if it's a palindrome
palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs

---- Exercises block 5, slides 360–362
-- define scalar product of two arrrays
scalar1 :: [Int] -> [Int] -> Int
scalar1 xs ys = sum (zipWith (*) xs ys)

scalar2 :: [Int] -> [Int] -> Int
scalar2 xs ys | length xs == length ys = sum [(xs !! i)*(ys !! i) | i <- [0..(length xs - 1)]]
              | otherwise = error "RTFM!"
