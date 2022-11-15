-- Exercise 1
tuple :: Monad m => m a -> m b -> m (a, b)
tuple xs ys = do x <- xs
                 y <- ys
                 return (x, y)
                  

tuple' :: Monad m => m a -> m b -> m (a, b)
tuple' xs ys = xs >>= \x -> 
               ys >>= \y -> 
               return (x, y)
       

-- Exercise 2
test :: Monad m => m t -> (t -> m a) -> (t -> b) -> m b
test z s f = do y <- z
                s y
                return ( f y )


test2 :: Monad m => m t -> (t -> m a) -> (t -> b) -> m b
test2 z s f = z >>= \y ->
              s y >>= \k ->
              return (f y)


-- Opgave 1.
fourfirst xs = do
    x <- xs
    return (4, x)
-- Den returnerer IKKE et par men en MONADISK STRUKTUR af par. I dette tilfælde er det en liste (giv skylden til teenageren og influenceren.)
-- x bliver genereret af xs - der kan derfor være mange.

