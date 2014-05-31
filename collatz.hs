collatz :: Int -> [Int]
collatz 1 = [1]
collatz x 
    | even x = x : collatz (div x 2) 
    | otherwise = x : collatz (x * 3 + 1)


filter' :: (a -> Bool) -> [a] -> [a]
filter' f  = foldr(\x acc -> if f x then x : acc else acc ) [] 


filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f [] = []


length (filter (>=15)(map length (map collatz [1..100])))


map collatz [1..100]

map length $ map collatz [1..100]


