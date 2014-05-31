foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f a (x:xs) = foldl' f (f a x) xs
foldl' f a [] = []