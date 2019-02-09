-------------------------------------------------------------------------------
-- |
-- Module    :  Data.LIFX.MultiZone
-- Copyright :  (c) Sam Stites 2017
-- License   :  BSD-3-Clause
-- Maintainer:  fnz@fgvgrf.vb (cipher:rot13)
-- Stability :  experimental
-- Portability: non-portable
--
-- MultiZone Messages
-------------------------------------------------------------------------------
module Data.LIFX.MultiZone where

import Prelude
import GHC.Word
import GHC.Int
import Data.ByteString

import Data.LIFX.Device
import Data.LIFX.Light

-- * Enumerated Types

-- | This type allows you to provide hints to the device about how the changes
-- you make should be performed. For example you can send multiple zones and
-- have them all apply at once. Application Request is stored as an unsigned
-- 8-bit integer.
data ApplicationRequest
  = NO_APPLY   -- ^ Value: 0.  Don't apply the requested changes until a message with APPLY or APPLY_ONLY is sent.
  | APPLY      -- ^ Value: 1.  Apply the changes immediately and apply any pending changes.
  | APPLY_ONLY -- ^ Value: 2.  Ignore the requested changes in this message and only apply pending changes.

-- * Messages

data Messages
  = SetColorZones Word8 Word8 HSBK Word32 ApplicationRequest -- ^ 501
  -- This message is used for changing the color of either a single or
  -- multiple zones. The changes are stored in a buffer and are only applied
  -- once a message with either APPLY or APPLY_ONLY set.
  --
  -- Field       | Type
  -- start_index | unsigned 8-bit integer
  -- end_index   | unsigned 8-bit integer
  -- color       | HSBK
  -- duration    | unsigned 32-bit integer
  -- apply       | Application Request

  | GetColorZones Word8 Word8 -- ^ 502
  -- GetColorZones is used to request the zone colors for a range of zones.
  -- The bulb will respond with either StateZone or StateMultiZone messages as
  -- required to cover the requested range. The bulb may send state messages
  -- that cover more than the requested zones. Any zones outside the requested
  -- indexes will still contain valid values at the time the message was sent.
  --
  -- Field       | Type
  -- start_index | unsigned 8-bit integer
  -- end_index   | unsigned 8-bit integer

  | StateZone Word8 Word8 HSBK -- ^ 503
  -- The StateZone message represents the state of a single zone with the
  -- index field indicating which zone is represented. The count field
  -- contains the count of the total number of zones available on the device.
  --
  -- Field       | Type
  -- count       | unsigned 8-bit integer
  -- index       | unsigned 8-bit integer
  -- color       | HSBK

  | StateMultiZone Word8 Word8 HSBK HSBK HSBK HSBK HSBK HSBK HSBK HSBK  -- ^ 506
  -- The StateMultiZone message represents the state of eight consecutive
  -- zones in a single message. As in the StateZone message the count field
  -- represents the count of the total number of zones available on the
  -- device. In this message the index field represents the index of color[0]
  -- and the rest of the colors are the consecutive zones thus the index of
  -- the color[n] zone will be index + n.
  --
  -- Field       | Type
  -- count       | unsigned 8-bit integer
  -- index       | unsigned 8-bit integer
  -- color[0]    | HSBK
  -- color[1]    | HSBK
  -- color[2]    | HSBK
  -- color[3]    | HSBK
  -- color[4]    | HSBK
  -- color[5]    | HSBK
  -- color[6]    | HSBK
  -- color[7]    | HSBK
