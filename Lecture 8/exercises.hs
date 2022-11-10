-- opg 1)
main = do
    w <- getLine
    loop ((read w) :: Int)
    where
        loop 1 = putStrLn (show 1)
        loop x = do
            putStrLn (show x)
            if even x
                then loop (x `div` 2)
                else loop (3*x + 1)
                
-- Opg 2)
letters :: IO ()
letters = do
    w <- getLine
    letter w
    where
        letter [] = return ()
        letter (x:xs) = do
                            putStrLn [x]       
                            letter xs
                            
-- Opg 3)
letters' :: IO ()
letters' = do
        w <- getLine
        sequence_ [putStrLn [x] | x <- w]

hugorm :: IO ()
hugorm = do
        putStr "How many numbers would you like to write? "
        w <- getLine
        result <- (hug ((read w) :: Int))
        putStrLn ("The sum is " ++ (show result))
        where
            hug 0 = return 0
            hug n = do
                    ww <- getLine
                    orm <- hug (n-1)
                    return (((read ww) :: Int) + orm)