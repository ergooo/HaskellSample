count :: (a -> Bool) -> [a] -> Int
count f xs = length $ foldr(\x acc -> if f x then x:acc else acc) [] xs