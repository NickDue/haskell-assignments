-- Opgave 3.1
pyt :: Int -> [(Int, Int, Int)]
pyt k = [(x,y,z) | x <- [1..k], y <- [1..k], z <- [1..k], x^2+y^2 == z^2, x <= y, y < z]

-- Opgave 3.2
sevens :: Int -> [Int]
sevens k = [x | x <- [1..k-1], x `mod` 7 == 0]

-- Opgave 3.3
headsup :: [Int] -> Bool
--headsup x = if head x == head (tail x) then True else False
headsup (x:xs) = x == head(xs)
-- The type is wrong, it needs to be of type Int and not of class Num

-- Opgave 3.4
plonk = \x -> (\y -> (\z -> x + y + z))
-- :t plonk = Num a => a -> a -> a -> a

-- Opgave 3.5
thishasnoname :: (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
thishasnoname x y (z, h) | x == y = min z h
                         | otherwise = max z h


-- Ekstra opgaver
-- Opgave a
flop :: [(b,a)] -> [(a,b)]
flop l = [(y, x) | (x, y) <- l]


-- Opgave b
dupli :: [a] -> [a]
dupli l = concat [[x,x] | x <- l]

-- Opgave c
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

--isperfect :: Int -> Bool
