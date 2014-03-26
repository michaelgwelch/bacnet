{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module BACnet.Reader.Core
  (
  Reader,
  peek,
  byte,
  bytes,
  sat,
  runReader,
  run
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.Functor.Identity
import Data.Word
import Control.Applicative hiding ((<|>))
import qualified Control.Applicative as Ap
import Control.Monad
import Text.Parsec.Prim
import Text.Parsec.Error
import Text.Parsec.Pos
import Numeric

-- | Defines the a new type that can be used to read Binary BACnet encoded
--   values
newtype Reader a = R { getParser :: Parsec BS.ByteString () a }

right :: Either a b -> b
right (Right x) = x

run :: Reader a -> [Word8] -> a
run r ws = right $ runR r (BS.pack ws)

runR :: Reader a -> BS.ByteString -> Either ParseError a
runR (R p) = parse p ""

runReader :: Reader a -> [Word8] -> Maybe(a, [Word8])
runReader (R p) bs =
  let parser = (p >>= \val ->
                getParserState >>= \s ->
                return (val, BS.unpack $ stateInput s)) in
  case parse parser "" (BS.pack bs) of
                      (Left _) -> Nothing
                      (Right x) -> Just x

instance (Monad m) => Stream BS.ByteString m Word8 where
  uncons = return . BS.uncons

updatePosWord8  :: SourcePos -> Word8 -> SourcePos
updatePosWord8 pos b
    = newPos (sourceName pos) (sourceLine pos) (sourceColumn pos + 1)

sat :: (Word8 -> Bool) -> Reader Word8
sat p =
  R (tokenPrim showByte nextPos textByte)
  where
    showByte = ("0x"++) . flip showHex ""
    nextPos p b _ = updatePosWord8 p b
    textByte b = if p b then Just b else Nothing

byte :: Reader Word8
byte = sat (const True)

peek :: Reader Word8
peek = R . lookAhead $ getParser byte

bytes :: Word8 -> Reader BS.ByteString
bytes 0 = return BS.empty
bytes n = BS.cons <$> byte <*> bytes (n-1)

readerBind :: Reader a -> (a -> Reader b) -> Reader b
readerBind (R pa) f = R (pa >>= \val ->
                         let (R pb) = f val in pb)

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
