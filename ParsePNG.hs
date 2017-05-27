module ParsePNG where

import System.Environment (getArgs)
import Data.ByteString as B

main :: IO ()
main =
     do path:_ <- getArgs
        raw <- B.readFile path
        print $ B.length raw
