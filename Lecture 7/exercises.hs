-- Opg 1

-- triples :  Num a => [( a, a, a)] −> ([a], [a], [a])
-- triples [] = ()
-- triples [ ( a , b , c ) ]= ( [ a ] , [ b ] , [ c ] )
-- triples ( x : xs , y : ys , z : z s ) = [ x , y , z ] : triples [ ( xs , ys , z s ) ]

-- 1: Base casen er forlert. Dem bør være en tuple med tre tomme lister
-- 2: Man kan ikke bruge cons-operatoren (:) på tupler
-- 3: osv...

-- Opg 2
example = [(1,2,3) , (4, 5, 6), (7, 8, 9)]

triples :: [(a,b,c)] -> ([a], [b], [c])
triples [] = ([],[],[])
-- triples (x:xs) = (k:kr, l:lr, j:jr) 
--             where 
--                 (k,l,j) = x
--                 (kr,lr,jr) = triples (xs)

triples ((k,l,j):xs) = (k:kr, l:lr, j:jr)
            where 
                (kr,lr,jr) = triples xs

-- Opg 3
-- DEN BLEV LAVET I LECTURE 4
-- MEN NU UDEN HEAD OG TAIL
example' =  [1,1,1,2,3,3,2] 
wrapup :: Eq a => [a] -> [[a]]
wrapup [] = [[]]
wrapup [x] = [[x]]
wrapup (x:xs) = if x == z
                    then (x : x') : xs'
                else 
                    [x] : x' : xs'
            where 
                (x':xs') = wrapup xs
                (z:zs) = x'


-- Opg 4
-- LAVET I LECTION 6
data Tree a = Leaf a | Empty | Node (Tree a) a (Tree a)

insert :: Ord a => Tree a -> a -> Tree a
insert Empty x = Leaf x
insert (Leaf y) x | y >= x = Node (Leaf x) y (Empty)
                  | otherwise = Node (Empty) y (Leaf x)
insert (Node lt y rt) x | y >= x = Node (insert lt x) y (rt)
                        | otherwise = Node (lt) y (insert rt x)

