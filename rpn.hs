solveRPN::String -> Double
solveRPN = head . foldl holdingFunction [] . words 

holdingFunction::[Double] -> String ->[Double] 
holdingFunction (x:y:xs) "+" = y + x : xs  
holdingFunction	(x:y:xs) "-" = y - x : xs
holdingFunction	(x:y:xs) "*" = y * x : xs
holdingFunction	(x:y:xs) "/" = y / x : xs
holdingFunction	stack v   = read v : stack