sumsq n = foldr ((+) . (^2)) 0 [1..n]

-- Exercise 1
within :: [Int] -> (Int, Int) -> [Int]
within xs (from,to) = filter (\x -> x >= from && x <= to) xs

--Exercise 2
example52 = [[1,2], [3,4]]
sumrows :: [[Int]] -> [Int]
sumrows = map sum

--Exercise 3
fact k = product [1..k]

--Exercise 4
fingo xs ys = foldr (:) xs ys

approx n = sum ( map (\x -> 1/fact x) [0..n] )

--Exercise a
filter2 p = foldr (\x xs-> if p x then x:xs else xs) [] 

--Exercise b
remove2 first = foldr (\x xs -> if x `elem` first then xs else x:xs) ""

--Exercise c
min2 ys = fst (foldr (\x (sl, l) -> if x < l then (l, x) else (if x < sl then (x, l) else (sl, l))) (maximum ys, maximum ys) ys)