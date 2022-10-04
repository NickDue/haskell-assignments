-- Opgave 1
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- Opgave 2
mylast :: [a] -> a
mylast (x:[]) = x
mylast (_:xs) = mylast xs

-- Opgave 3
wrapup :: Eq a => [a] -> [[a]]
wrapup [] = [[]]
wrapup [x] = [[x]]
wrapup (x:xs) = if x == head (head ys)
                then (x:head ys):(tail ys) 
                else [x]:ys
                where
                    ys = wrapup xs
-- Opgave 4
rle :: Eq a => [a] -> [(a,Int)]
rle [] = []
rle (x:xs) = if x == fs then (x, v):ys else (f : ys)
            where 
                (f : ys) = rle xs
                (fs, v) = f
                

-- Opgave a
isolate :: Eq a => [a] -> a -> ([a], [a])
isolate [] _ = ([], [])
isolate (l:ls) x = (l1, l2)
    where
        l1 = [l | l /= x] ++ fst (isolate ls x)
        l2 = [x | l == x] ++ snd (isolate ls x)