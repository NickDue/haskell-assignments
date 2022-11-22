import Parsing


-- Exercise 2:
-- expr ::= term | term ◦ expr | term ∪ expr 
-- term ::= (expr)* | factor
-- factor ::= a | b 


-- Exercise 3:
data Rexp = A | B | Union Rexp Rexp | Concat Rexp Rexp | Kstar (Rexp) deriving Show

expr = do x <- term
          char 'O'
          y <- expr
          return (Concat x y)
       <|>
       do x <- term
          char 'U'
          y <- expr
          return (Union x y)
       <|> term

term = do char '('
          x <- expr
          char ')'
          char '*'
          return (Kstar (x))
       <|> factor

factor = do char 'a'
            return A
         <|>
         do char 'b'
            return B