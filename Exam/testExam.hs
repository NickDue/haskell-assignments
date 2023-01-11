-- Problem 1:

-- 1.

-- fx saved in fun, but else x is used for comparison
-- where x=x, should be == and x will always be equal to x
-- then Nothing is wrong, we don't return nothing
-- else x : (allAnswers f xs) should be else Just x : (allAnswers f xs)

-- 2.

allAnswers :: Eq a => (b -> Maybe a) -> [b] -> Maybe [a]
                
allAnswers f xs = if (any (== Nothing) fxs) then Nothing else Just (map fromJust fxs)
                  where fxs = (map f xs)
                        fromJust (Just x) = x
                  

-- 3
allAnswers' f [] = Just []
allAnswers' f (x:xs) = do 
                    z <- allAnswers' f xs
                    v <- f x
                    return (v:z)

test _ = Nothing



-- Problem 2:

-- 1

-- a
opg2a :: Eq a1 => a2 -> (a1, a1) -> [a2]

opg2a x (y,z) = if y == z then [x] else []


-- b

opg2b :: Eq a => (a -> a -> Bool) -> Bool -> a -> a -> Bool

opg2b f x y z = x && f y z

test2 _ _ = True

-- c
opg2c :: Show a => a -> IO b -> IO b
opg2c x y = do 
            z <- y 
            putStrLn (show x)
            return z

-- d
opg2d :: (a -> b) -> a -> a -> [b]
opg2d f x y = [f x, f y]
test3 z = z + 1


-- Opgave 3
data Tree a = Leaf a | Node (Tree a) (Tree a) | Empty
treeExample = Node (Leaf "dog") (Node (Leaf "cat") (Leaf "hamster"))

minimax :: Ord a => Tree a -> (a,a)
minimax (Leaf x) = (x,x)
minimax (Node l r) = (lowest, highest)
                    where 
                        (minl, maxl) = minimax l
                        (minr, maxr) = minimax r
                        lowest = min minl minr
                        highest = max maxl maxr

-- Opgave 4
echo =  putStr "Please type a word: " >>
        getLine >>= \s ->
        putStrLn ("You typed " ++ s)
        
seconds = do
            s <- getLine
            let z = read s :: [(Bool, Bool)]
            return [ x | (_,x) <- z]

-- Opgave 5
-- 1: In a list every element has to be of the same type. If you want more than one type, you can make your own datatype or encapsulate the elements further
-- 2:
data Alternating a b = Tom | Pair a b (Alternating a b) deriving Show
myalt = Pair 5 True (Pair 6 False (Pair 7 True (Tom))) 

seperating (Tom) = ([],[])
seperating (Pair x y z) = (x : s, y : seperate)
            where
                (s, seperate) = seperating z
                
infalt = Pair 1 "a" (more 2)
            where
                more x = Pair x (replicate x 'a') (more (x+1))


-- Opgave 6
newtype ToPairs a = TP (a,a) deriving Show

-- 1;
pairOne = TP (True, False)
pairTwo = TP(opg61, opg61)
opg61 Nothing = 0
opg61 (Just x) = 5 + x

-- 2:
instance Functor ToPairs where
    fmap g ( TP (x, y) ) = TP (g x, g y)

-- 3:
instance Applicative ToPairs where
    pure x = TP (x,x)
    (TP (f,g)) <*> (TP (x,y)) = TP (f x, g y)
