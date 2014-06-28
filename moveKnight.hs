import Control.Monad

type KnightPos = (Int, Int)

moveKnight::KnightPos -> [KnightPos]
moveKnight (x,y) = [
   (x+2,y+1), (x+2,y-1), 
   (x-2,y+1), (x-2,y-1), 
   (x+1,y+2), (x+1,y-2), 
   (x-1,y+2), (x-1,y-2)
   ] >>= (\(n,m) -> guard(n > 0 && m > 0 && n < 9 && m < 9) >> return (n,m))

in3 :: KnightPos -> [KnightPos]
in3 start = do
	first <- moveKnight start
	second <- moveKnight first
	moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start