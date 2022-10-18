-- Discussion problems
-- 1) 
data Unary = Z | I Unary

unary2int :: Unary -> Integer
unary2int Z = 0
unary2int (I n) = 1 + unary2int n

-- 2)
data Tree a = Leaf a | Node (Tree a) a (Tree a)

tree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
tree2 = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Node (Leaf 5) 9 (Leaf (-1))))

least (Leaf x) = x
least (Node l x r) = min (least l) (min x (least r))