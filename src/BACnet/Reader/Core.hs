module BACnet.Reader.Core
  (
  Reader,
  byte,
  bytes,
  sat,
  runReader
  ) where

import Data.Word
import Control.Applicative

newtype Reader a = R {runReader :: [Word8] -> Maybe (a, [Word8])}

success :: a -> Reader a
success a = R (\inp -> Just(a, inp))

failure :: Reader a
failure = R (\inp -> Nothing)

bindReader :: Reader a -> (a -> Reader b) -> Reader b
bindReader ra f =
  R (\inp -> case runReader ra inp of
              Nothing -> Nothing
              Just(val, out) -> runReader (f val) out)

byte :: Reader Word8
byte = R (\inp -> case inp of
                    [] -> Nothing
                    (b:bs) -> Just(b, bs))

(+++) :: Reader a -> Reader a -> Reader a
r1 +++ r2 = R (\inp -> case runReader r1 inp of
                          Nothing -> runReader r2 inp
                          result -> result)

bytes :: Word8 -> Reader [Word8]
bytes 0 = return []
bytes n = pure (:) <*> byte <*> bytes (n-1)

sat :: (Word8 -> Bool) -> Reader Word8
sat pred = byte >>= \b -> if pred b then return b else failure

instance Monad Reader where
  (>>=) = bindReader
  return = success
  fail _ = failure

rmap :: (a -> b) -> Reader a -> Reader b
rmap f ra = ra >>= \a ->
            return $ f a

instance Functor Reader where
  fmap = rmap

readerSeq :: Reader (a -> b) -> Reader a -> Reader b
readerSeq rf ra = rf >>= \f ->
                  ra >>= \a ->
                  return $ f a

instance Applicative Reader where
  pure = success
  (<*>) = readerSeq

instance Alternative Reader where
  empty = failure
  (<|>) = (+++)
