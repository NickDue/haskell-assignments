allbutsecond :: [a] -> [a]
allbutsecond [] = []
allbutsecond [x] = [x]
allbutsecond (x:xs) = x : tail xs

midtover :: [a] -> [a]
