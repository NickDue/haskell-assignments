-- Discussion 1:
data Onion a = Core a | Layer ( Onion a ) deriving Show

instance Functor Onion where
    fmap g (Core x) = Core (g x)
    fmap g (Layer x) = Layer (fmap g x)

insOni = Layer (Layer(Layer (Core "bingo")))

-- Discussion 2:
-- Yes it is

-- Exercises
-- Opgave 1
data UTree a = Node a [UTree a] deriving Show

instance Functor UTree where
   -- fmap g (Node x []) = Node (g x)
    fmap g (Node x ys) = Node (g x) [fmap g y | y <- ys]
    -- fmap g (Node x ys) = Node (g x) (fmap (fmap g) ys)  ANDEN LØSNING UDEN LIST COMPREHENSION

example1 = Node 1 [Node 2 [Node 3 []]]

--instance Functor ((->)r) where
    --fmap :: (a -> b) -> (r -> a) -> (r -> b)

[] <**> _ = []
_ <**> [] = []
(g:gs) <**> xs = fmap g xs ++ gs <**> xs