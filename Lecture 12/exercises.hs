-- Discussion problems:
-- 1)
nsonly n = map (*n) [0..]
nsonly' x n = x * n : nsonly' (x+1) n
nsonly'' n = nsonlyrec 0
         where
                nsonlyrec x = x * n : nsonlyrec (x+1)

-- Opgave 1
indflet :: a -> [a] -> [a]
indflet _ [] = []
indflet _ [x] = [x]
indflet e (x:y:ys) = x : e : indflet e (y:ys)
-- head (indflet 1 (2:undefined))
-- Vi mener at den giver en exception, for hvis ét element er undefined og det element evalueres, vil alt evalueres til undefined.

-- Opgave 2
fletind :: a -> [a] -> [a]
fletind _ [] = []
fletind e (x:xs) =  x : e : fletind e xs
-- head (fletind 1 (2:undefined))

-- Opgave 3
-- [”0”,”1”,”01”,”11”,”001” ,... ]
allBinaries :: [String]
allBinaries = "0" : [toBinary x | x <- [1..]]

toBinary 0 = ""
toBinary x = (if x `mod` 2 == 1
                then "1"
             else "0") ++ toBinary (x `div` 2)