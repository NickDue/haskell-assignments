-- Opgave 1.1 - All but second element
allbutsecond [] = []
allbutsecond [a] = [a]
allbutsecond (x:xs) = [x] ++ tail xs

allbutsecond' [] = []
allbutsecond' [a] = [a]
allbutsecond' (x:xs) = x : tail xs


-- Opgave 1.2 - Midtover

--midtover [] = ([], [])
--midtover [a] = ([], [a])

-- Løsning 1 (Den smarte)
midtover x = (take p x, drop p x)
            where p = length x `div` 2

-- Løsning 2 (Den "dovne udgave")
midtover' x = splitAt (length x `div` 2) x


-- Opgave 1.3 - Spot fejlen
--   bingo (x,y) = x mod z 
--   where
--   z = y + 42

--  Der mangler indents og så skal der være backticks ` rundt om "mod", så det står som `mod`



-- EKSTRA OPGAVER
-- a) Function that returns the last element of a list
endelement [] = []
endelement x = head(reverse x)

endelement' [] = []
endelement' x = last x

-- b) qsort in descending order
-- Simple just do 
    -- reverse (qsort list)

