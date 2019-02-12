-------------------------------------------------------------------------------
-- |
-- Module    :  Data.LIFX.Header
-- Copyright :  (c) Sam Stites 2017
-- License   :  BSD-3-Clause
-- Maintainer:  fnz@fgvgrf.vb (cipher:rot13)
-- Stability :  experimental
-- Portability: non-portable
--
-- All LIFX Protocol messages start with the header.
--
-- * Header Description
--
-- Each LIFX Protocol message has the following format
--
--     Frame | Frame Address | Protocol Header | Payload
--
-- The header is composed of the Frame, Frame Address and Protocol header. The
-- Payload is covered separately in the documentation for the various message
-- types.
-------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Data.LIFX.Header
  ( Word(..)
  , Frame, frame
  , FrameAddress, frameAddress
  , ProtocolHeader, protocolHeader
  ) where

import Prelude hiding (Word, sequence)

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Data.Bits
import Data.ByteString (ByteString)
import Data.Coerce
import Control.Monad hiding (sequence)
import Data.Proxy
import GHC.Generics
import GHC.Word hiding (Word(..))
import GHC.TypeLits
import qualified Data.ByteString as BS

newtype Word (rep :: Nat) = Word Word64
  deriving (Eq, Ord, Show, Generic)

instance KnownNat r => Serialise (Word r) where
  encode :: forall r . KnownNat r => Word r -> Encoding
  encode (Word s) = foldMap (encodeBool . testBit s) [0..fromIntegral (natVal (Proxy @r)) - 1]

  decode :: forall r s . KnownNat r => Decoder s (Word r)
  decode = do
    bools <- replicateM (fromIntegral (natVal (Proxy @r))) decodeBool
    pure $ Word $ foldl (\r (i, b) -> if b then setBit r i else r) zeroBits $ zip [0..] bools

-- The Frame section contains information about the following:
--   Size of the entire message
--   LIFX Protocol number: must be 1024 (decimal)
--   Use of the Frame Address target field
--   Source identifier
--
-- Total bits: 16+2+1+1+12+32
data Frame = Frame
  { size        :: Word16          -- ^ Size of entire message in bytes including this field
  , origin      :: Word 2          -- ^ Message origin indicator: must be zero (0)
  , tagged      :: Bool            -- ^ Determines usage of the Frame Address target field
  , addressable :: Bool            -- ^ Message includes a target address: must be one (1)
  , protocol    :: Word 12         -- ^ Protocol number: must be 1024 (decimal)
  , source      :: Word32          -- ^ Source identifier: unique value set by the client, used by responses
  } deriving (Eq, Ord, Show, Generic)

frame
  :: (size ~ Word16, tagged ~ Bool, source ~ Word32)
  => size -> tagged -> source -> Frame
frame sz t = Frame sz (Word 0) t True (Word 1024)

instance Serialise Frame where
  encode :: Frame -> Encoding
  encode f
    =  encodeWord16 (size f)
    <> encode       (origin f)
    <> encodeBool   (tagged f)
    <> encodeBool   (addressable f)
    <> encode       (protocol f)
    <> encodeWord32 (source f)

  decode :: Decoder s Frame
  decode = Frame
    <$> decodeWord16
    <*> decode
    <*> decodeBool
    <*> decodeBool
    <*> decode
    <*> decodeWord32

-- The Frame Address section contains the following routing information:
--   Target device address
--   Acknowledgement message is required flag
--   State response message is required flag
--   Message sequence number
--
-- Total bits: 64+48+6+1+1+8
data FrameAddress = FrameAddress
  { target       :: Word64    -- ^ 6 byte device address (MAC address) or zero (0) means all devices
  , reserved0    :: Word 48   -- ^ Must all be zero (0)
  , reserved1    :: Word 6    -- ^ Reserved
  , ack_required :: Bool      -- ^ Acknowledgement message required
  , res_required :: Bool      -- ^ Response message required
  , sequence     :: Word8     -- ^ Wrap around message sequence number
  } deriving (Eq, Ord, Show)

frameAddress
  :: (target ~ Word64, ack_required ~ Bool, res_required ~ Bool, sequence ~ Word8)
  => target -> ack_required -> res_required -> sequence -> FrameAddress
frameAddress t a b c = FrameAddress t (Word 0) (Word 0) a b c

instance Serialise FrameAddress where
  encode :: FrameAddress -> Encoding
  encode f
    =  encodeWord64 (target       f)
    <> encode       (reserved0    f)
    <> encode       (reserved1    f)
    <> encodeBool   (ack_required f)
    <> encodeBool   (res_required f)
    <> encodeWord8  (sequence     f)

  decode :: Decoder s FrameAddress
  decode = FrameAddress
    <$> decodeWord64
    <*> decode
    <*> decode
    <*> decodeBool
    <*> decodeBool
    <*> decodeWord8

-- The Protocol header contains the following information about the message:
--   Message type which determines what action to take (based on the Payload)
data ProtocolHeader = ProtocolHeader
  { reserved2 :: Word64   -- ^ Reserved
  , mtype     :: Word16   -- ^ Message type determines the payload being used
  , reserved3 :: Word16   -- ^ Reserved
  } deriving (Eq, Ord, Show)

protocolHeader :: (Word16 ~ mtype) => mtype -> ProtocolHeader
protocolHeader t = ProtocolHeader 0 t 0

instance Serialise ProtocolHeader where
  encode :: ProtocolHeader -> Encoding
  encode f
    =  encodeWord64 (reserved2 f)
    <> encodeWord16 (mtype     f)
    <> encodeWord16 (reserved3 f)

  decode :: Decoder s ProtocolHeader
  decode = ProtocolHeader
    <$> decodeWord64
    <*> decodeWord16
    <*> decodeWord16

newtype Payload = Payload ByteString
  deriving (Eq, Ord, Show, Generic)


