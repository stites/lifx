-------------------------------------------------------------------------------
-- |
-- Module    :  Data.LIFX.Tile
-- Copyright :  (c) Sam Stites 2017
-- License   :  BSD-3-Clause
-- Maintainer:  fnz@fgvgrf.vb (cipher:rot13)
-- Stability :  experimental
-- Portability: non-portable
--
-- Tile Messages
--
-- These messages are specific to controlling and querying LIFX Tile devices.
-- See the page on Tiles for more information about these products.
-------------------------------------------------------------------------------
module Data.LIFX.Tile where

import Prelude
import GHC.Word
import GHC.Int
import Data.ByteString

import Data.LIFX.Light

-- * Message Field Data Types

-- The device_version fields are the same as those in a Device::StateVersion
-- message and the the firmware_build and firmware_version fields are the same
-- as those in a Device::StateHostFirmware message.
--
-- The width and height fields are a number of pixels that are on each axis of
-- the tile.
--
-- The user_x and user_y fields are used to record the position of each tile.
-- See the page on Tile Control for more information about user_x and user_y
--
-- The accel_meas fields are the gravity measurements on X,Y and Z axes. The
-- measurement range is ±2g with a sensitivity of 4096 LSB/g.
--
-- The following javascript is an example of determining the rotation of the
-- device from those measurements.
--
-- > function determine_rotation(x, y, z) {
-- >   var absX = abs(x)
-- >   var absY = abs(y)
-- >   var absZ = abs(z)
-- >
-- >   if (x == -1 && y == -1 && z == -1) {
-- >     // Invalid data, assume right-side up.
-- >     return "rightSideUp"
-- >
-- >   } else if (absX > absY && absX > absZ) {
-- >     if (x > 0) {
-- >       return "rotateRight"
-- >     } else {
-- >       return "rotateLeft"
-- >     }
-- >
-- >   } else if (absZ > absX && absZ > absY) {
-- >     if (z > 0) {
-- >       return "faceDown"
-- >     } else {
-- >       return "faceUp"
-- >     }
-- >
-- >   } else {
-- >     if (y > 0) {
-- >       return "upsideDown"
-- >     } else {
-- >       return "rightSideUp"
-- >     }
-- >   }
-- > }
data Tile = Tile
  { accel_meas_x :: Int16
  , accel_meas_y :: Int16
  , accel_meas_z :: Int16
  , reserved     :: Int16
  , user_x       :: Float
  , user_y       :: Float
  , width        :: Word8
  , height       :: Word8
  , reserved0    :: Word8
  , device_version_vendor  :: Word32
  , device_version_product :: Word32
  , device_version_version :: Word32
  , firmware_build         :: Word64
  , reserved1              :: Word64
  , firmware_version       :: Word32
  , reserved2              :: Word32
  } deriving (Eq, Ord, Show)

-- * Tile Messages

data TileMessages
  = GetDeviceChain -- ^ 701
  -- This message returns information about the tiles in the chain. It responds with a StateDeviceChain message.

  | StateDeviceChain Word8 [Tile] Word8 -- ^ 702
  -- Field             | Type
  -- start_index       | unsigned 8-bit integer
  -- tile_devices[16]  | 16 Tile
  -- total_count       | unsigned 8-bit integer

  | SetUserPosition Word8 Word16 Float Float -- ^ 703
  -- Used to tell each tile what their position is.
  --
  -- Field      | Type
  -- tile_index | unsigned 8-bit integer
  -- reserved   | unsigned 16-bit integer
  -- user_x     | 32-bit float
  -- user_y     | 32-bit float
  --
  -- The tile_index is a 0 based index used to address a particular tile in the chain.
  --
  -- The user_x and user_y fields are used to record the position of each tile.
  -- See the page on Tile Control for more information about user_x and user_y.
  --
  -- Warning!
  --
  -- Be very careful when setting these values, as it has the chance to
  -- greatly upset users if you get it wrong. Make sure you have read, and
  -- fully understand the information on the Tile Control page before setting
  -- these values.

  | GetTileState64 Word8 Word8 Word8 Word8 Word8 Word8 -- ^ 707
  -- Get the state of 64 pixels in the tile in a rectangle that has a starting
  -- point and width.
  --
  -- Field       | Type
  -- tile_index  | unsigned 8-bit integer
  -- length      | unsigned 8-bit integer
  -- reserved    | unsigned 8-bit integer
  -- x           | unsigned 8-bit integer
  -- y           | unsigned 8-bit integer
  -- width       | unsigned 8-bit integer
  --
  -- The tile_index is used to control the starting tile in the chain and
  -- length is used to get the state of that many tiles beginning from the
  -- tile_index. This will result in a separate response from each tile.
  --
  -- For the LIFX Tile it really only makes sense to set x and y to zero, and width to 8.

  | StateTileState64 Word8 Word8 Word8 Word8 Word8 [HSBK] -- ^ 711
  -- Returned from a GetTileState64 and provides all the pixels in the
  -- specified rectangle for that tile.
  --
  -- Field       | Type
  -- tile_index  | unsigned 8-bit integer
  -- reserved    | unsigned 8-bit integer
  -- x           | unsigned 8-bit integer
  -- y           | unsigned 8-bit integer
  -- width       | unsigned 8-bit integer
  -- colors[64]  | 64 HSBK values

  | SetTileState64 Word8 Word8 Word8 Word8 Word8 Word8 Word32 [HSBK] -- ^ 715
  -- This lets you set 64 pixels from a starting x and y for a rectangle with the specified width.
  --
  -- For the LIFX Tile it really only makes sense to set x and y to zero, and width to 8.
  --
  -- Field       | Type
  -- tile_index  | unsigned 8-bit integer
  -- length      | unsigned 8-bit integer
  -- reserved    | unsigned 8-bit integer
  -- x           | unsigned 8-bit integer
  -- y           | unsigned 8-bit integer
  -- width       | unsigned 8-bit integer
  -- duration    | unsigned 32-bit integer
  -- colors[64]  | 64 HSBK values

