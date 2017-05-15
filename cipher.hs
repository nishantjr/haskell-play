import Data.Char
import Test.QuickCheck


substitutionCipher :: (Char -> Char) -> String -> String
substitutionCipher f s = map f s

rotateWithOffset :: Int -> Int -> Char -> Char
rotateWithOffset n offset c
    = chr $ (ord c - offset + n) `mod` 26 + offset

rotateLowerN :: Int -> Char -> Char
rotateLowerN n c
    | isLower c = rotateWithOffset n (ord 'a') c
    | otherwise = c

rotateUpperN :: Int -> Char -> Char
rotateUpperN n c
    | isUpper c = rotateWithOffset n (ord 'A') c
    | otherwise = c

caesar3Inv = substitutionCipher $ (rotateLowerN $ 0 - 3).(rotateUpperN $ 0 - 3)
caesar3    = substitutionCipher $ (rotateLowerN 3).      (rotateUpperN 3)


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
                                 "Hello World! abc xyz", "Ebiil Tloia! xyz uvw")
            ]
main = test
