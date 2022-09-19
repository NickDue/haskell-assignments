-- Opgave 2.1

twice :: (a -> a) -> a -> a
twice f x = f (f (x))
-- it uses parametric polymorhpism since we do not declare a data type.

-- Opgave 2.2
dingo :: (a,a) -> [a]
dingo (x,y) = [x,y]
-- it uses parametric polymorhpism since we do not declare a data type.

-- Opgave 2.3
-- bighead :: Ord a => [a] -> Int

-- Opgave 2.4
-- You cannot be sure that two functions are equal. HINT: EQ_tm is undecideable and un-recognizable

-- Ekstra Opgaver

-- Opgave a
mango :: Num a => a -> a -> a -> a
mango x y z = x * y + z - 42
-- mango 14 :: Num a => a -> a -> a

-- Opgave b
bingo :: a -> a
bingo a = a

--Opgave c
thesame :: Eq a => [(a, a)] -> [(a,a)]
thesame xs = [(x,y) | (x,y) <- xs, x == y]

-- Opgave d
[(+), (*), (+), (-)] :: Num a => [a -> a -> a] -- A list of binary operators
[(+), (*), (+), (-), (++)] :: Num [a] => [[a] -> [a] -> [a]]
