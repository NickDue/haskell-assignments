-- Ex 1
letrec sum = λx. if x = 0 then 0
                 else
                   plus x (sum (minus x 1))
in
      sum 4

-- Ex 2
data LExp = Var Integer | Abs String | App LExp LExp | Let String LExp LExp | If LExp LExp LExp |
            LetRec String LExp LExp | Const Integer
-