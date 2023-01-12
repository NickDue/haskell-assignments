-- Lecture 1
-- 1.1
example11 = [1,2,3,4]
allbutsecond (x:xs) = x : tail xs

-- 1.2
example12 = [1,2,3,4,5]
midtover xs = splitAt x xs
            where
                x = length xs `div` 2

-- 1.3
example13 = (2,3)
bingo (x,y) = x `mod` z
            where
                z = y + 42

-- Lecture 2 - Types and type classes
-- 2.1
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 2.2
dingo :: (a,a) -> [a]
dingo (x,y) = [x,y]

-- 2.3
example23 = [7,4,5,8,9]
bighead :: Ord a => [a] -> Int
bighead (x:xs) = length ([y | y <- xs, y > x])

-- Lecture 3
-- 3.1
pyt :: Int -> [(Int, Int, Int)]
pyt k = [(x,y,z) | x <- [1..k], y <- [1..k], z <- [1..k], x^2+y^2 == z^2, x <= y, y < z]

-- 3.2
sevens :: Int -> [Int]
sevens k = [x | x <- [1..k-1], x `mod` 7 == 0]

-- 3.3
headsup :: [Int] -> Bool
--headsup x = if head x == head (tail x) then True else False
headsup (x:xs) = x == head xs
-- The type is wrong, it needs to be of type Int and not of class Num

-- 3.4
plonk = \x -> (\y -> (\z -> x + y + z))
-- :t plonk = Num a => a -> a -> a -> a

-- 3.5
thishasnoname :: (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
thishasnoname x y (z, h) | x == y = min z h
                         | otherwise = max z h


-- Lecture 4 - Recursion
-- 4.1
example41 = [1,2,3,4]
myrev :: [a] -> [a]
myrev [] = []
myrev [x] = [x]
myrev (x:xs) = myrev xs ++ [x]

-- 4.2
mylast [x] = x
mylast (x:xs) = mylast xs

-- 4.3
example43 = [1,1,1,2,3,3,2]
wrapup [] = [[]]
wrapup [x] = [[x]]
wrapup (x:xs) = if x == x'' then (x:x'):xs' else [x]:(x':xs')
            where
                (x':xs') = wrapup xs
                (x'':_) = x'

-- 4.4
example44 = [ 1 , 1 , 1 , 2 , 2 , 1 , 3 , 3 ]
rle :: Eq a => [a] -> [(a,Int)]
rle [] = []
rle [x] = [(x,1)]
rle (x:xs) = if x == fs then (x, v + 1):ys else (x,1):(f : ys)
            where 
                (f : ys) = rle xs
                (fs, v) = f

-- 4.5
example45 = [(1,2,3), (4,5,6), (7,8,9)]
triples :: Num a => [(a,a,a)] -> ([a],[a],[a])
triples [] = ([],[],[])
triples [(a,b,c)] = ([a],[b],[c])
triples ((x,y,z):xs) = (x:x', y:y', z:z')
                where
                    (x', y', z') = triples xs

-- Lecture 5 - High order functions
-- 5.1
within :: [Int] -> (Int, Int) -> [Int]
within xs (from,to) = filter (\x -> x >= from && x <= to) xs

-- 5.2
example52 :: [[Int]]
example52 = [[1,2], [3,4]]
sumrows :: [[Int]] -> [Int]
sumrows = map sum

-- 5.3
fact k = product [1..k]
approx :: (Fractional a, Enum a) => a -> a
approx n = sum ( map (\x -> 1/fact x) [0..n] )

-- 5.4
example54a = [1,2,3]
example54b = [4,5,6] 
fingo :: Foldable t => [a] -> t a -> [a]
fingo xs ys = foldr (:) xs ys

-- Lecture 6 - Declare your own types and classes
-- 6.1
data Aexp = N Integer | Var String | Add Aexp Aexp | Mult Aexp Aexp deriving (Show)
example61 = Add (N 9) (Mult (N 7) (N 2))

-- 6.2
type Assoc k v = [(k, v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

eval :: Aexp -> Assoc String Integer -> Integer
eval (N n) _ = n
eval (Var v) lt = find v lt
eval (Add x y) lt = eval x lt + eval y lt
eval (Mult x y) lt = eval x lt * eval y lt

lookupTable = [("x",8), ("y",5)]

-- 6.3
data Dir = File String Integer | Subdir String [Dir]
example2 = Subdir "Hej" [Subdir "Hej2" [], File "kasper" 42069]


-- 6.4
data Tree a = Leaf a | Empty | Node (Tree a) a (Tree a)

insert :: Ord a => Tree a -> a -> Tree a
insert Empty x = Leaf x
insert (Leaf y) x | y >= x = Node (Leaf x) y (Empty)
                  | otherwise = Node (Empty) y (Leaf x)
insert (Node lt y rt) x | y >= x = Node (insert lt x) y (rt)
                        | otherwise = Node (lt) y (insert rt x)



-- 6.5
example65a = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))
example65b = Node (Node (Leaf 1) 2 (Leaf 3)) 4 Empty
balanced :: Tree a -> Bool
balanced Empty = True
balanced (Leaf x) = True
balanced (Node x y z) = balanced x && balanced z && nodeBalance
                where
                    leavesLeft = allleaves x
                    leavesRight = allleaves z
                    nodeBalance = leavesLeft == leavesRight+1 || leavesLeft == leavesRight-1 ||leavesLeft == leavesRight

allleaves Empty = 0
allleaves (Leaf x) = 1
allleaves (Node x y z) = allleaves x + allleaves z


-- Lecture 8 - Interactive programming
-- 8.1
-- Irrelevant assignment

--  8.2
letters :: IO ()
letters = do
    w <- getLine
    letter w
    where
        letter [] = return ()
        letter (x:xs) = do
                            putStrLn [x]       
                            letter xs

-- 8.3
letters' :: IO ()
letters' = do
        w <- getLine
        sequence_ [putStrLn [x] | x <- w]

-- 8.4
hugorm :: IO ()
hugorm = do
        putStr "How many numbers would you like to write? "
        w <- getLine
        result <- (hug ((read w) :: Int))
        putStrLn ("The sum is " ++ (show result))
        where
            hug 0 = return 0
            hug n = do
                    ww <- getLine
                    orm <- hug (n-1)
                    return (((read ww) :: Int) + orm)

-- Lecture 9 - Functors
-- 9.1
data UTree a = UNode a [UTree a] deriving Show

instance Functor UTree where
   -- fmap g (Node x []) = Node (g x)
    fmap g (UNode x ys) = UNode (g x) [fmap g y | y <- ys]
