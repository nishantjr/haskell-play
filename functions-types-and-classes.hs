double :: Num a => a -> a
double x = x + x

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' :: Int -> Int
factorial' n= product [1..n]

isIn :: (Eq a) => [a] -> a -> Bool
isIn [] _ = False
isIn (x:xs) a
    | x == a    = True
    | otherwise = isIn xs a

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
    | isIn xs x =     unique xs
    | otherwise = x : unique xs

powerset :: [Char] -> [[Char]]
powerset []  = [[]]
powerset [x] = [[]] ++ [[x]]
powerset (x:xs) = unique (map ([x] ++) (powerset xs) ++ powerset [x] ++ powerset xs)
