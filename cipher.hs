import Data.Char
import Test.QuickCheck


substitutionCipher :: (Char -> Char) -> String -> String
substitutionCipher f s = map f s

-- Composable library of substitutions ------------------
swapChars :: Char -> Char -> Char -> Char
swapChars a b char
    | char == a     = b
    | char == b     = a
    | otherwise     = char

swapLowerUpper c
    | isUpper c = toLower c
    | isLower c = toUpper c
    | otherwise = c

rotateLowerN :: Int -> Char -> Char
rotateLowerN n c
    | isLower c = rotateWithOffset n (ord 'a') c
    | otherwise = c

rotateUpperN :: Int -> Char -> Char
rotateUpperN n c
    | isUpper c = rotateWithOffset n (ord 'A') c
    | otherwise = c

rotateN n = (rotateLowerN n).(rotateUpperN n)

caesarN n  = substitutionCipher $ rotateN n

caesar3Inv = caesarN $ 0 - 3
caesar3    = caesarN 3

--- Helpers ------------------------------------------
rotateWithOffset :: Int -> Int -> Char -> Char
rotateWithOffset n offset c
    = chr $ (ord c - offset + n) `mod` 26 + offset

--- Testing helpers ----------------------------------
testToUpper = substitutionCipher toUpper

prop_caesarUncaesar :: String -> Bool
prop_caesarUncaesar s = (caesar3 . caesar3Inv) s == s

--- Test Framework -----------------------------------
runTest :: (String, (String -> String), String, String) -> String
runTest (name, f, input, expected)
        | expected == actual    = "passed: " ++ name
        | otherwise             = "FAILED: " ++ name ++ ": " ++ expected ++
                                  " ≠ " ++ actual
    where actual = f input
test :: IO ()
test = do mapM_ (putStrLn.runTest) tests
--          quickCheck prop_caesarUncaesar
--          toLower doesn't work as expected -- '\246' "is lower"
   where tests = [
                ("toUpper", testToUpper, "Hello World!", "HELLO WORLD!"),
                ("caesar3", caesar3    , "Hello World!", "Khoor Zruog!"),
                ("caesar3Inv", caesar3Inv,
                                 "Hello World! abc xyz", "Ebiil Tloia! xyz uvw"),
                ("complexCipher", substitutionCipher $
                    (swapChars '!' '@').(swapChars ' ' '&').
                    swapLowerUpper.(rotateN 13),
                    "Hello World! abc xyz", "uRYYB&jBEYQ@&NOP&KLM")
            ]
main = test
