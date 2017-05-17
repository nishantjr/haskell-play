type Parser a = String -> [ (a, String) ]
parse :: Parser a -> String -> [ (a, String) ]
parse p s = p s

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
    []         -> []
    [(v, out)] -> parse (f v) out

---

succeed :: a -> Parser a
succeed v = \inp -> [ (v, inp) ]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item inp = case inp of
  []      -> []
  (x:xs)  -> [ (x, xs) ]

---


p :: Parser Char
p = do x <- item
       succeed('x')
--       succeed(x)
--       y <- item
--       (succeed (x, y))
