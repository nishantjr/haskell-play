{-# LANGUAGE RebindableSyntax #-}

import Prelude (IO, String, Char, Maybe (Nothing, Just),
    print, fromInteger, const, ($))

type Parser a = String -> Maybe (a, String)

parse :: Parser a -> String -> Maybe (a, String)
parse parser s = parser s

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f =
    \s -> case parse p s of
               Nothing      -> Nothing
               Just (x, s') -> parse (f x) s'

(>>) :: Parser a -> Parser b -> Parser b
p >> q = p >>= const q

return :: a -> Parser a
return x = \s -> Just (x, s)

fail :: Parser a
fail = \s -> Nothing

char :: Parser Char
char = \s -> case s of x:xs -> Just (x, xs)
                       ""   -> Nothing

secondChar :: Parser Char
secondChar =
     do char
        char

firstThird :: Parser (Char, Char)
firstThird = do x <- char
                char
                y <- char
                return (x, y)

main :: IO ()
main =
-- print $ (return 3) "hello"
-- print $ char "hello"
-- print $ secondChar "hello"
   print $ firstThird "hello"
