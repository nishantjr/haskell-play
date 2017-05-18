{-# LANGUAGE RebindableSyntax #-}

import Prelude (
    IO, String, Char, Maybe (Nothing, Just),
    print, putStrLn, fromInteger, const, ($), (++), (==), Bool (True, False), Int, read,
    foldr, repeat, take, fst)
import Data.Char

--- XXX
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ a = a


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

sat :: Parser a -> (a -> Bool) -> Parser a
sat p f = do x <- p
             if f x then return x
             else fail

literal :: Char -> Parser Char
literal c = sat char (== c)

digit :: Parser Char
digit = sat char isDigit

letter :: Parser Char
letter = sat char isLetter

(+++) :: Parser a -> Parser a -> Parser a
a +++ b = \s -> case parse a s of Nothing -> parse b s
                                  x       -> x

many1 :: Parser a -> Parser [a]
many1 p = do x  <- p
             xs <- many p
             return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

nat :: Parser Int
nat = do ds <- many digit
         return (read ds)

expanderator :: Parser String
expanderator = do segments <-  many termExpandor
                  return (foldr (++) [] segments)
    where termExpandor :: Parser String
          termExpandor = do n <- nat
                            c <- char
                            return (take n (repeat c))

secondChar :: Parser Char
secondChar =
     do char
        char

firstThird :: Parser (Char, Char)
firstThird = do x <- digit +++ letter
                char
                y <- char
                return (x, y)

main :: IO ()
main =
-- print $ (return 3) "hello"
-- print $ char "hello"
-- print $ secondChar "hello"
-- print $ (many (digit +++ letter)) "2sdd"
   putStrLn $ case expanderator expr of
                    Just (x, s) -> x
                    Nothing     -> "Oops."
              where expr = "2 1\\1\n3 1\\1\n2 1/1 1\\1\n1 1/3 1\\1\n1/5 1\\1\n" 
