-- Opgave 1
data Aexp = N Integer | Var String | Add Aexp Aexp | Mult Aexp Aexp deriving (Show)
example = Add (N 9) (Mult (N 7) (N 2))

-- Opgave 2
type Assoc k v = [(k, v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

eval :: Aexp -> Assoc String Integer -> Integer
eval (N n) _ = n
eval (Var v) lt = find v lt
eval (Add x y) lt = eval x lt + eval y lt
eval (Mult x y) lt = eval x lt * eval y lt

lookupTable = [("x",8), ("y",5)]


-- Opgave 3
-- data Dir a String b Int = Empty Null | Mult Dir Dir | Subdir Dir
data Dir = File String Integer | Subdir String [Dir]
example2 = Subdir "Hej" [Subdir "Hej2" [], File "kasper" 42069]

-- Opgave 4
data Tree a = Leaf a | Empty | Node (Tree a) a (Tree a)

insert :: Ord a => Tree a -> a -> Tree a
insert Empty x = Leaf x
insert (Leaf y) x | y >= x = Node (Leaf x) y (Empty)
                  | otherwise = Node (Empty) y (Leaf x)
insert (Node lt y rt) x | y >= x = Node (insert lt x) y (rt)
                        | otherwise = Node (lt) y (insert rt x)


-- Opgave a
