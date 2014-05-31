zip' :: [a] -> [b] -> [(a,b)]
zip' a b = zipWith mktuple a b

mktuple :: a -> b -> (a,b)
mktuple a b = (a,b)
