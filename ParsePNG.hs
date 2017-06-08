module ParsePNG where

import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Char (ord, chr)
import Data.Word (Word8, Word32)
import Data.Bits
import System.Environment (getArgs)

main :: IO ()
main =
     do args <- getArgs
        let path = head args
        raw <- B.readFile path
        print $ runParser ((parseError :: String -> Parser ()) "I'm an error!") raw
        print $ runParser lookahead raw
        print $ runParser consumeByte raw
        print $ runParser getByte raw
        print $ runParser (constByte 109) raw
        print $ runParser ((constByte 109) >> (constByte 122)) raw
        print $ runParser (constChar 'm') raw
        print $ runParser (constString "module") raw
        case runParser pngParser raw of
            Left  err -> putStrLn err
            Right (png, bytesConsumed) -> do
                print png
                print $ size png
                print $ colorType png
                print $ palette png
                print bytesConsumed

----------------------------------------------------------------------
-- PNG Data Structure

data PNG = PNG
    { size :: (Word32, Word32)
    , colorType :: ColorType
    , palette  :: Maybe [ (Word8, Word8, Word8) ]  -- List of RGBs
    }
    deriving Show

data ColorType = Grayscale | RGB | Palette | GrayscaleAlpha | RGBAlpha
    deriving Show

data Chunk = Chunk
    { cType :: B.ByteString
    , cData :: B.ByteString
    }
    deriving Show

----------------------------------------------------------------------
-- Parser Infrastructure 

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

runParser :: Parser a -> B.ByteString -> Either String (a, Word)
runParser (Parser parser) bytes =
    case parser init of
        Left  (err, state) -> Left $ errMsg err state
        Right (x, state)   -> Right (x, bytesConsumed state)
    where
        init = ParseState bytes 0
        errMsg err state = err ++ " at byte " ++ show (bytesConsumed state)

data ParseState = ParseState
    {  remainder                :: B.ByteString
    ,  bytesConsumed            :: Word
    }

----------------------------------------------------------------------
-- Parsers

pngParser :: Parser PNG
pngParser = do
               header
               ihdrLen <- word32
               if ihdrLen == 13 then return ()
                                else parseError $
                                    "IHDR chunk must be 13 bytes" ++
                                    " got: " ++ (show ihdrLen)
               constString "IHDR"
               width <- word32
               height <- word32
               _bitDepth <- word8
               tempColorType <- colorTypeP
               _compression <- word8
               _filterMethod <- word8
               _interlaceMethod <- word8
               _crc <- word32
               chunk  -- sRGB
               chunk  -- gAMA
               chunk  -- cHRM
               palette <- optional paletteChunk
               return $ PNG (width, height) tempColorType palette

header :: Parser ()
header  = do constByte 0x89; constString "PNG"; cr; lf; ctrlZ; lf

colorTypeP :: Parser ColorType
colorTypeP = do      (constByte 0 >> return Grayscale)
                .||. (constByte 2 >> return RGB)
                .||. (constByte 3 >> return Palette)
                .||. (constByte 4 >> return GrayscaleAlpha)
                .||. (constByte 6 >> return RGBAlpha)
                .||. (do byte <- lookahead
                         parseError $ "Bad colorType byte " ++ show byte)

paletteChunk :: Parser [ (Word8, Word8, Word8) ]
paletteChunk = do length <- word32
                  constString "PLTE"
                  chunkData <- binaryData $ word32ToInt length
                  _crc <- word32
                  return $ triplets $ B.unpack chunkData
    where triplets (r:g:b:rest) = (r, g, b):(triplets rest)
          triplets []           = []

chunk :: Parser Chunk
chunk   = do length <- word32
             chunkType <- binaryData 4
             chunkData <- binaryData $ word32ToInt length
             word32
             return (Chunk chunkType chunkData)

parseError :: String -> Parser a
parseError message = Parser $ \state -> Left (message, state)

(.||.) :: Parser a -> Parser a -> Parser a
Parser p .||. Parser q = Parser $ \state -> case p state of
                                Left (err, state') -> q state
                                Right x          -> Right x
optional :: Parser a -> Parser (Maybe a)
optional (Parser p) = Parser $ \state -> case p state of
                                Left (err, state') -> Right(Nothing, state)
                                Right(x, state') -> Right(Just x, state')

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

constByte :: Word8 -> Parser ()
constByte b = do b' <- lookahead
                 if b == b' then consumeByte
                            else parseError $
                                   "expected " ++ show b ++ " got " ++ show b'

word8 :: Parser Word8
word8 = do b <- lookahead
           consumeByte
           return b

word8sToWord32 :: [Word8] -> Word32
word8sToWord32 = foldl accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

word32 :: Parser Word32
word32 = fmap word8sToWord32 parserOfList
    where listOfParser = take 4 (repeat word8)
          parserOfList = sequence listOfParser

binaryData :: Int -> Parser B.ByteString
binaryData len = fmap B.pack $ sequence (take len (repeat word8))

word8ToChar :: Word8 -> Char
word8ToChar = chr.fromIntegral.toInteger

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral.toInteger

constChar    :: Char -> Parser ()
constChar    = constByte.fromIntegral.ord

constString :: String -> Parser ()
constString want = Parser $ \state -> case fBinaryData state of
                    Left x -> Left x
                    Right (got, state') ->
                       if pack want == got then Right ((), state')
                       else Left("expected " ++ want ++ " got " ++ show got, state)
        where Parser fBinaryData = binaryData $ length want

cr      = constByte 0x0d
lf      = constByte 0x0a
ctrlZ   = constByte 0x1a
