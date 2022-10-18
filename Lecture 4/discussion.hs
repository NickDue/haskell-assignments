rep :: Int -> a -> [a]
rep 0 _ = []
rep n x = [x] ++ rep (n-1) x

improve :: [a] -> [a]
improve [] = []
improve (x:[]) = [x]
improve (x:xs) = [x] ++ improve (tail xs)