{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- |
-- Provides the primitives necessary for building readers of
--   'BS.ByteString' or ['Word8'] data.
-- A reader attempts to read from an input stream. The reader can succeed
-- or fail depending on the input stream and what it is attempting to read.


module BACnet.Reader.Core
  (
  --
  -- * Introduction
  -- $Introduction

  Reader,

  -- * The Runners
  run,
  runR,
  runS,

  -- * The Primitive Readers
  peek,
  byte,
  bytes,
  bytestring,
  sat,
  getInputStream,
  getInputState,
  getPos,
  getPos',

-- * About the Instances
-- $Instances
  ) where

import qualified Data.ByteString as BS
import Data.Word
import Control.Applicative
import Control.Monad
import Numeric
import qualified Data.Attoparsec.ByteString as P

-- FRAGILE - Using Internals of Attoparsec
import qualified Data.Attoparsec.Internal.Types as AT


-- | @Reader a@ is a type of parser with return type of @a@.
newtype Reader a = R { getParser :: P.Parser a }


-- | Runs the specified reader on the given input. If successful
--   it returns the value that was read. If unsuccessful, an error
--   is thrown giving the location of the error and the unexpected 'Word8'
--   value
run :: Reader a -> [Word8] -> a
run r = done . P.parse (getParser r) . BS.pack 

done :: P.Result a -> a
done (P.Fail rem _ m) = error m 
done (P.Done _ r) = r
done partial = done $ P.feed partial BS.empty -- Recurse to get an error

-- | Runs the specified reader on the given input. It returns the
--   result in a @P.Result a@
runR :: Reader a -> BS.ByteString -> ParseResult a
runR (R p) = fromResult . P.parse p  

data ParseResult a = Success a [Word8]
  | Error [Word8] [String] String
  | NotDone (BS.ByteString -> P.Result a) 

instance Show a => Show (ParseResult a) where
  show (Success r bs) = show (r, bs)
  show (Error bs context message) = show (bs, context, message)
  show (NotDone f) = show $ P.Partial f

fromResult :: P.Result a -> ParseResult a
fromResult (P.Done remain result) = Success result $ BS.unpack remain
fromResult (P.Partial f) = NotDone f
fromResult (P.Fail remain context message) = 
    Error (BS.unpack remain) context message

runS :: Reader a -> [Word8] -> ParseResult a
runS r = runR r . BS.pack

-- | The reader @sat f@ succeeds for any byte for which the supplied function
--   @f@ returns 'True'. Returns the byte that is actually read.
sat :: (Word8 -> Bool) -> Reader Word8
sat = R . P.satisfy

-- | This reader succeeds for any byte that is read. Returns the read byte.
byte :: Reader Word8
byte = R P.anyWord8

-- | @peek@ reads a byte and returns it without consuming any input.
-- The following examples use 'runR' which if the reading is successful
-- returns the remaing list of bytes as part of the result.
--
-- >>> runS peek [0, 1, 2]
-- (0,[0,1,2])
--
-- This differs from 'byte' which consumes input like so:
--
-- >>> runS byte [0, 1, 2]
-- (0,[1,2])
peek :: Reader Word8
peek = R P.peekWord8'

-- | @bytes n@ reads and consumes the next n bytes. Returns the bytes
--   as a ['Word8'].
bytes :: Word8 -> Reader [Word8]
bytes n = BS.unpack <$> bytestring n

-- | @bytestring n@ reads and consumes the next n bytes. Returns the
--   bytes in a 'BS.ByteString'.
bytestring :: Word8 -> Reader BS.ByteString
bytestring = R . P.take . fromIntegral


readerBind :: Reader a -> (a -> Reader b) -> Reader b
readerBind ra f = R $ getParser ra >>= getParser . f

-- | A reader that returns the remaing input stream.
getInputStream :: Reader BS.ByteString
getInputStream = R P.takeByteString

-- | A reader that returns the current state of the input stream. In other words,
-- it returns the portion of the stream that has not yet been consumed. It
-- is just like 'getInputStream' except it unpacks the 'BS.ByteString' returned
-- by 'getInputStream'.
--
-- >>>run getInputState [1,2,3,4]
-- [1,2,3,4]
--
-- >>>run (byte >> getInputState) [1,2,3,4]
-- [2,3,4]
getInputState :: Reader [Word8]
getInputState = BS.unpack <$> getInputStream


-------
-- Note this part is fragile. It's using the internals of
-- attoparsec. This is only used for failure messages. If it
-- breaks, an easy fix is to not worry about position
-------
getPos :: Reader Int
getPos = R $ AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

getPos' :: Reader String
getPos' = show <$> getPos


instance Monad Reader where
  fail = R . fail
  (>>=) = readerBind
  return = R . return

instance Functor Reader where
  fmap = liftM

instance Applicative Reader where
  pure = return
  (<*>) = ap

instance Alternative Reader where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Reader where
  mzero = fail "mzero"
  (R p1) `mplus` (R p2) = R (p1 `mplus` p2)

-- $Introduction
-- This module defines the core primitives for a Reader.
--
-- There are several functions
-- that can be used to run a reader: 'run', 'runR'. They differ
-- in what input they take, and in how they handle failure. For example,
-- 'runR' returns an @Either ParserError a@ so that it can return failure
-- information or the result. The function 'run' expects the reader to
-- succeed and throws an error if it fails. It's primarily meant for testing.
--
-- The simplest
-- readers are 'sat', 'byte', 'bytes', 'bytestring', 'peek'.
--
-- Here are some examples that
-- show how to run these readers.
--
-- >>> import Data.ByteString as BS
-- >>> runR byte (BS.pack [0x23, 0x34])
-- Right 35
--
-- >>> runR (sat (==0)) (BS.pack [0x23, 0x34])
-- Left (line 1, column 1):
-- unexpected 0x23
--
-- If you expect your readers to always succeed you can use 'run' instead
-- of 'runR'. Recall, an error is thrown if the reading is not successful.
-- The 'run' function also differs in how the input is given. It expects a
-- ['Word8'] instead of a 'BS.ByteString'
--
-- >>> run byte [10, 20]
-- 10
--
-- >>> run (sat (==0)) [0x23]
-- *** Exception: (line 1, column 1):
-- unexpected 0x23
--
-- In some instances (testing, for example) it's nice to see how much of
-- the input stream is left after running a 'Reader'. This can be done with
-- 'runR'. In the following example 5 bytes are read and 5 are yet to be
--  consumed.
--
-- >>> runR (bytes 5) [0,1,2,3,4,5,6,7,8,9]
-- Right ([0,1,2,3,4],[5,6,7,8,9])
--
--

-- $Instances
-- The 'Reader' type is an instance of several classes: 'Monad', 'Functor',
-- 'MonadPlus', 'Applicative' and 'Alternative'.
--
-- The 'MonadPlus' instance will be described now. The function
-- 'mzero' is defined to be a reader failure.
--
-- >>> run mzero [0x01, 0x02]
-- *** Exception: (line 1, column 1):
-- mzero
--
-- The function 'mplus' allows you to try reading input with two different
-- readers. The first reader is run. If that succeeds then the value
-- it read is returned. Only if it fails is the second reader run. If it
-- succeeds then its value is returned. If the second reader also fails
-- then the whole read fails.
--
-- Note, the first reader may consume input even
-- if the reading fails. If
-- this is not desired, wrap the first reader with 'try'. This
-- first example shows how even though the first reader fails, the second
-- one succeeds.
--
-- >>> runR (mzero `mplus` byte) [1, 2]
-- Right (1,[2])
--
-- In the following example the second reader should succeed, except in trying
-- the first reader all the input was consumed. Therefore, the second reader
-- and the overall read represented by the @mplus@ also failed.
--
-- >>> runR (bytes 3 `mplus` bytes 2) [1, 2]
-- Left (line 1, column 3):
-- unexpected end of input
--
-- This can be resolved, if desired, by wrapping the first reader with try:
--
-- >>> runR (try (bytes 3) `mplus` bytes 2) [1,2]
-- Right ([1,2],[])
--
-- The 'Alternative' instance works just like the 'MonadPlus' instance
-- with 'empty' equivalent to 'mzero' and 'Control.Applicative.<|>' equivalent to 'mplus'.
--
-- The instances for 'Monad', 'Functor', and 'Applicative' all work as
-- expected. Here are some examples:
--
-- In this example we use '<$>' (an alias for 'fmap') to multiply the
-- byte we read by 2.
--
-- >>> run ((*2) <$> byte) [1]
-- 2
--
-- This example shows the use of 'Applicative'
--
-- >>> run ((*) <$> byte <*> byte) [3,7]
-- 21
--
-- Finally, an example of 'Monad'.
--
-- >>> run (byte >>= \b -> if b < 5 then bytes 2 else bytes 3) [0, 2, 4, 5]
-- [2,4]
--
-- >>> :{
-- let reader =
--       do
--         b <- peek
--         bs <- bytes b
--         return $ Prelude.map (*2) bs
-- in run reader [3,2,1]
-- :}
-- [6,4,2]


