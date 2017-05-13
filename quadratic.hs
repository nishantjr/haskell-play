getInt :: IO Int
getInt = do
    sa <- getLine
    return (read sa)

main = do
    putStrLn "ax^2 + bx + c?"
    a <- getInt
    b <- getInt
    c <- getInt
    putStrLn ("b^2 - 4 a c = " ++ (show (b * b - 4 * a * c)))
