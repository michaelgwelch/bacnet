{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Provides the primitives necessary for building readers of
--   'BS.ByteString' or ['Word8'] data.
-- A reader attempts to read from an input stream. The reader can succeed
-- or fail depending on the input stream.
--
-- There are serval functions
-- that can be used to run a reader: 'run', 'runR', 'runS'. They differ
-- in what input they take, and in how they handle failure. For example,
-- 'runR' returns an @Either ParserError a@ so that it can return failure
-- information or the result. The function 'run' expects the reader to
-- succeed and throws an error if it fails. It's primarily meant for testing.
--
-- The simplest
-- readers are 'sat', 'byte', 'bytes', 'bytestring', 'peek'.
--
-- Here are some examples that
-- show how to use the simplest readers.
--
-- >>> import Data.ByteString.Lazy as BS
-- >>> runR byte (BS.pack [0x23, 0x34])
-- Right 35
--
-- >>> runR (sat (==0)) (BS.pack [0x23, 0x34])
-- Left (line 1, column 1):
-- unexpected 0x23
--
-- If you expect your readers to always succeed you can use 'run' instead
-- of 'runR'. However, an error is thrown if the reading is not successful.
-- The 'run' function also differs in how the input is given. It expects a
-- ['Word8'] instead of a 'BS.ByteString'
--
-- >>> run byte [10, 20]
-- 10
--
-- >>> run (sat (==0)) [0x23]
-- *** Exception: (line 1, column 1):
-- unexpected 0x23
module BACnet.Reader.Core
  (
  Reader,
  run,
  runR,
  runS,
  peek,
  byte,
  bytes,
  bytestring,
  sat,
  try,
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.Word
import Control.Applicative
import Control.Monad
import Text.Parsec.Prim hiding (try)
import qualified Text.Parsec.Prim as Pr
import Text.Parsec.Error
import Text.Parsec.Pos
import Numeric

-- | @Reader a@ is a type of parser with return type of @a@.
newtype Reader a = R { getParser :: Parsec BS.ByteString () a }

right :: Either ParseError b -> b
right (Right x) = x
right (Left err) = error $ show err

-- | Runs the specified reader on the given input. If successful
--   it returns the value that was read. If unsuccessful, an error
--   is thrown giving the location of the error and the unexpected 'Word8'
--   value
run :: Reader a -> [Word8] -> a
run r = right . runR r . BS.pack

-- | Runs the specified reader on the given input. It returns the
--   result in an @Either ParserError a@
runR :: Reader a -> BS.ByteString -> Either ParseError a
runR (R p) = parse p ""

returnStream :: Parsec BS.ByteString () a -> Parsec BS.ByteString() (a, BS.ByteString)
returnStream p =
  do
    val <- p
    s <- getParserState
    return (val, stateInput s)

-- | Like 'run' it runs the specified reader with the given list of bytes.
--   If it succeeds it return the leftover input as part of the return value.
runS :: Reader a -> [Word8] -> Either ParseError (a, [Word8])
runS (R p) inp =
  case runR (R (returnStream p)) (BS.pack inp) of
    Right (v, bs) -> Right (v, BS.unpack bs)
    Left pe -> Left pe


instance (Monad m) => Stream BS.ByteString m Word8 where
  uncons = return . BS.uncons

updatePosWord8  :: SourcePos -> Word8 -> SourcePos
updatePosWord8 pos b
    = newPos (sourceName pos) (sourceLine pos) (sourceColumn pos + 1)

-- | The reader @sat f@ succeeds for any byte for which the supplied function
--   @f@ returns 'True'. Returns the byte that is actually read.
sat :: (Word8 -> Bool) -> Reader Word8
sat p =
  R (tokenPrim showByte nextPos testByte)
  where
    showByte = ("0x"++) . flip showHex ""
    nextPos p b _ = updatePosWord8 p b
    testByte b = if p b then Just b else Nothing

-- | This reader succeeds for any byte that is read. Returns the read byte.
byte :: Reader Word8
byte = sat (const True)

-- | @peek@ reads a byte and returns it without consuming any input.
-- The following examples use 'runS' which if the reading is successful
-- returns the remaing list of bytes as part of the result.
--
-- >>> runS peek [0, 1, 2]
-- Right (0,[0,1,2])
--
-- This differs from 'byte' which consumes input like so:
--
-- >>> runS byte [0, 1, 2]
-- Right (0,[1,2])
peek :: Reader Word8
peek = R . lookAhead $ getParser byte

-- | @bytes n@ reads and consumes the next n bytes.
bytes :: Word8 -> Reader [Word8]
bytes 0 = return []
bytes n = (:) <$> byte <*> bytes (n-1)


bytestring :: Word8 -> Reader BS.ByteString
bytestring 0 = return BS.empty
bytestring n = BS.cons <$> byte <*> bytestring (n-1)

readerBind :: Reader a -> (a -> Reader b) -> Reader b
readerBind (R pa) f = R (pa >>= \val ->
                         let (R pb) = f val in pb)

try :: Reader a -> Reader a
try = R . Pr.try . getParser

instance Monad Reader where
  fail _ = R $ parserFail ""
  (>>=) = readerBind
  return v = R (parserReturn v)

instance Functor Reader where
  fmap = liftM

instance Applicative Reader where
  pure = return
  (<*>) = ap

instance Alternative Reader where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Reader where
  mzero = fail ""
  (R p1) `mplus` (R p2) = R (p1 `mplus` p2)
