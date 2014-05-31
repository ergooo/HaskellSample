filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr(\x acc -> if f x then x : acc else acc ) []