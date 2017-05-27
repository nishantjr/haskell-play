module ParsePNG where

import Data.ByteString as B
import Data.Char (ord)
import Data.Word (Word8)
import System.Environment (getArgs)

main :: IO ()
main =
     do path:_ <- getArgs
        raw <- B.readFile path
        print $ runParser (parseError "I'm an error!") raw
        print $ runParser lookahead raw
        print $ runParser consumeByte raw
        print $ runParser getByte raw
        -- print $ runParser (byte 109) raw
        -- print $ runParser (byte 108) raw

-- On failure, return an error message and the old state
newtype Parser a = Parser
    (ParseState -> Either (String, ParseState) (a, ParseState))

instance Functor Parser where
    fmap f (Parser p) = Parser $ \state ->
        case p state of
             Left  x           -> Left x
             Right (x, state') -> Right (f x, state')

instance Applicative Parser where
    pure x = Parser $ \state -> Right (x, state)
    (Parser p) <*> (Parser q) = Parser $ \state ->
        case p state of
            Left  x           -> Left x
            Right (f, state') ->
                case q state' of
                    Left  x            -> Left  x
                    Right (x, state'') -> Right (f x, state'')

instance Monad Parser where
    (Parser p) >>= f = Parser $ \state ->
        case p state of Left x -> Left x
                        Right (x, state') -> let (Parser q) = f x in q state'


runParser :: Parser a -> ByteString -> Either String (a, Int)
runParser (Parser parser) bytes =
    case parser init of
        Left  (err, _) -> Left err
        Right (x, state)   -> Right (x, B.length (remainder state))
    where
        init = ParseState bytes

data ParseState = ParseState
    {  remainder                :: ByteString
    }

parseError :: String -> Parser ()
parseError message = Parser $ \state -> Left (message, state)

lookahead :: Parser Word8
lookahead = Parser $ \state ->
    if B.null (remainder state)
        then Left  ("No bytes left to parse", state)
        else Right (B.head (remainder state), state)

consumeByte :: Parser ()
consumeByte = Parser $ \state ->
    if B.null (remainder state)
        then Left  ("No bytes left to parse", state)
        else Right ((), ParseState $ B.tail $ remainder state)

getByte = do b <- lookahead
             consumeByte
             return b

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
