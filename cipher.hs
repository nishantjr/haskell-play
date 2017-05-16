import Data.Char
import Data.List
import Test.QuickCheck

substitutionCipher :: (Char -> Char) -> String -> String
substitutionCipher = map

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
    | isLowerEnglish c = rotateWithOffset n (ord 'a') c
    | otherwise = c

rotateUpperN :: Int -> Char -> Char
rotateUpperN n c
    | isUpperEnglish c = rotateWithOffset n (ord 'A') c
    | otherwise = c

rotateN n = (rotateLowerN n).(rotateUpperN n)

caesarN n  = substitutionCipher $ rotateN n

caesar3Inv = caesarN $ 0 - 3
caesar3    = caesarN 3

--- Validate substitutions ---------------------------

isBijection :: (Char -> Char) -> Bool
isBijection f = 256 == length (nub $ map (f.chr) [0..255])

--- Helpers ------------------------------------------
rotateWithOffset :: Int -> Int -> Char -> Char
rotateWithOffset n offset c
    = chr $ (ord c - offset + n) `mod` 26 + offset

isLowerEnglish :: Char -> Bool
isLowerEnglish c
    | c >= 'a' && c <= 'z'      = True
    | otherwise                 = False
isUpperEnglish :: Char -> Bool
isUpperEnglish c
    | c >= 'A' && c <= 'Z'      = True
    | otherwise                 = False

--- Testing helpers ----------------------------------
testToUpper = substitutionCipher toUpper

prop_caesarUncaesar :: String -> Bool
prop_caesarUncaesar s = (caesar3 . caesar3Inv) s == s

--- Test Framework -----------------------------------
runTestCiphers :: (String, (String -> String), String, String) -> String
runTestCiphers (name, f, input, expected)
        | expected == actual    = "passed: " ++ name
        | otherwise             = "FAILED: " ++ name ++ ": " ++ expected ++
                                  " ≠ " ++ actual
    where actual = f input

runValidateSubst :: (String, (Char -> Char), Bool) -> String
runValidateSubst (name, f, expected)
        | expected == actual    = "passed: valid: " ++ name
        | otherwise             = "FAILED: valid: " ++ name ++ ": "
                                 ++ (show expected) ++ " ≠ " ++ (show actual)
    where actual = isBijection f
test :: IO ()
test = do mapM_ (putStrLn.runTestCiphers) cipherSpecs
          mapM_ (putStrLn.runValidateSubst) substValidSpec
          putStr "QuickCheck: "; quickCheck prop_caesarUncaesar
   where cipherSpecs = [
                ("toUpper", testToUpper, "Hello World!", "HELLO WORLD!"),
                ("caesar3", caesar3    , "Hello World!", "Khoor Zruog!"),
                ("caesar3Inv", caesar3Inv,
                                 "Hello World! abc xyz", "Ebiil Tloia! xyz uvw"),
                ("complexCipher", substitutionCipher $
                    (swapChars '!' '@').(swapChars ' ' '&').
                    swapLowerUpper.(rotateN 13),
                    "Hello World! abc xyz", "uRYYB&jBEYQ@&NOP&KLM")
            ]
         substValidSpec = [
               ("identity", id, True),
               ("toUpper", toUpper, False),
               ("toLower", toLower, False),
               ("rotate3", rotateN 3, True),
               ("rotate25", rotateN 25, True),
               ("rotate3Inv", rotateN $ 0 - 3, True)
            ]
main = test
