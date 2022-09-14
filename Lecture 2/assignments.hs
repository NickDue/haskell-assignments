-- Opgave 1.1 - All but second element
allbutsecond [] = []
allbutsecond [a] = [a]
allbutsecond (x:xs) = [x] ++ tail xs

allbutsecond' [] = []
allbutsecond' [a] = [a]
allbutsecond' (x:xs) = x : tail xs


-- Opgave 1.2 - Midtover

midtover [] = ([], [])
midtover [a] = ([], [a])
--midtover x = (take p x, drop p x)
--            where p = length x `div` 2
midtover x = splitAt (length x `div` 2) x