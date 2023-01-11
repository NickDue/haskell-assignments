-- Lecture 1: First steps
allbutsecond :: [a] -> [a]
allbutsecond [] = []
allbutsecond [x] = [x]
allbutsecond (x:xs) = x : tail xs 

midtover :: [a] -> ([a],[a])
midtover [] = ([],[])
midtover [x] = ([x], [])
midtover xs = splitAt (length xs `div` 2) xs