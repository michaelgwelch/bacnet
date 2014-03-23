module BACnet.Reader.Core
  (
  Reader,
  byte,
  sat,
  runReader
  ) where

import Data.Word

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

sat :: (Word8 -> Bool) -> Reader Word8
sat pred = byte >>= \b -> if pred b then return b else failure

instance Monad Reader where
  (>>=) = bindReader
  return = success
  fail _ = failure
