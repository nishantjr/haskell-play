module ParsePNG where

import qualified Data.ByteString as B
import Data.Char (ord)
import Data.Word (Word8)
import System.Environment (getArgs)

main :: IO ()
main =
     do args <- getArgs
        let path = Prelude.head args
        raw <- B.readFile path
        print $ runParser (parseError "I'm an error!") raw
        print $ runParser lookahead raw
        print $ runParser consumeByte raw
        print $ runParser getByte raw
        print $ runParser (byte 109) raw
        print $ runParser ((byte 109) >> (byte 122)) raw
        print $ runParser (char 'm') raw
        print $ runParser (string "module") raw
        print $ runParser header raw

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


runParser :: Parser a -> B.ByteString -> Either String (a, Int)
runParser (Parser parser) bytes =
    case parser init of
        Left  (err, state) -> Left $ errMsg err state
        Right (x, state)   -> Right (x, B.length (remainder state))
    where
        init = ParseState bytes 0
        errMsg err state = err ++ " at byte " ++ show (bytesConsumed state)

data ParseState = ParseState
    {  remainder                :: B.ByteString
    ,  bytesConsumed            :: Word
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
        else Right ((),
                ParseState (B.tail$ remainder state) (bytesConsumed state + 1))

getByte = do b <- lookahead
             consumeByte
             return b

byte :: Word8 -> Parser ()
byte b = do b' <- lookahead
            if b == b' then consumeByte
                       else parseError $
                           "expected " ++ show b ++ " got " ++ show b'

char    = byte.fromIntegral.ord
string :: String -> Parser [()]
string  = mapM char
cr      = byte 0x0d
lf      = byte 0x0a
ctrlZ   = byte 0x1a
header  = do byte 0x89; string "PNG"; cr; lf; ctrlZ; lf
