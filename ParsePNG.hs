module ParsePNG where

import Data.ByteString as B
import Data.Char (ord)
import Data.Word (Word8)
import System.Environment (getArgs)

main :: IO ()
main =
     do path:_ <- getArgs
        raw <- B.readFile path
        print $ runParser getByte raw
        -- print $ runParser (byte 109) raw
        -- print $ runParser (byte 108) raw

-- On failure, return an error message and the old state
type Parser a = ParseState -> Either (String, ParseState) (a, ParseState)

{-
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = \state -> case p state of
                              Left x -> Left x
                              Right (x, state') -> Right (f x, state')
-}

runParser :: Parser a -> ByteString -> Either String a
runParser parser bytes =
    case parser init of
        Left  (err, _) -> Left err
        Right (x, _)   -> Right x
    where
        init = ParseState bytes

data ParseState = ParseState
    {  remainder                :: ByteString
    }

parseError :: String -> Parser a
parseError message state = Left (message, state)

lookahead :: Parser Word8
lookahead state =
    if B.null bytes then Left  ("No bytes left to parse", state)
                    else Right (b, state)
    where
        bytes  = remainder state
        b      = B.head bytes

consumeByte :: Parser ()
consumeByte state =
    if B.null bytes then Left  ("No bytes left to parse", state)
                    else Right ((), state')
    where
        bytes  = remainder state
        state' = ParseState $ B.tail $ remainder state

getByte :: Parser Word8
getByte state =
    if B.null bytes then Left  ("No bytes left to parse", state)
                    else Right (b, state')
    where
        bytes  = remainder state
        b      = B.head bytes
        state' = ParseState $ B.tail $ remainder state
{-
getByte state =
     do b <- lookahead
        consumeByte
        return b
-}

byte :: Word8 -> Parser ()
byte = undefined
{-
byte n  =
     do b <- lookahead
        if n == b then getByte >>= return ()
                  else parseError "Expected n got b"


char c  = byte.ord
cr      = byte 0x0d
lf      = byte 0x0a
ctrlZ   = byte 0x1a
string  = mapM char
header  = do byte 0x89; string "PNG"; cr; lf; ctrlZ; lf
-}
