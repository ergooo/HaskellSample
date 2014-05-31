data Tree a = EmptyTree | Node (Tree a) a (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node EmptyTree x EmptyTree

treeInsert :: (Ord a) =>a ->Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node left a right)
	| x == a = Node left a right
	| x > a  = Node left a (treeInsert x right)
	| x < a  = Node (treeInsert x left) a right

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node left a right)
	| x == a = True
	| x > a = treeElem x right
	| x < a = treeElem x left
