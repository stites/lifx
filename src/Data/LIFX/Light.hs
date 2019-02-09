-------------------------------------------------------------------------------
-- |
-- Module    :  Data.LIFX.Light
-- Copyright :  (c) Sam Stites 2017
-- License   :  BSD-3-Clause
-- Maintainer:  fnz@fgvgrf.vb (cipher:rot13)
-- Stability :  experimental
-- Portability: non-portable
-- Light Messages
--
-- Light messages control and acquire the state of a specific type of device,
-- such as a lightbulb. The state is comprised of the label, power level and
-- color.
--
-- There are a duplicate set of device power related messages,
-- Device::GetPower, Device::StatePower and Device::SetPower, which are
-- extended for lights by adding a duration field to Light::SetPower.
--
-- The Protocol header message type field value (decimal) appears after each
-- message name in this document. Each section below describes the payload that
-- is appended to the message header. Some messages do not require a payload.
-------------------------------------------------------------------------------
module Data.LIFX.Light where

import Prelude
import GHC.Word
import GHC.Int
import Data.ByteString

import Data.LIFX.Device

-- *  Message Field Data Types

-- | HSBK is used to represent the color and color temperature of a light.
--
-- The color is represented as an HSB (Hue, Saturation, Brightness) value.
--
-- The color temperature is represented in K (Kelvin) and is used to adjust the
-- warmness / coolness of a white light, which is most obvious when saturation
-- is close zero.
--
-- For user interface purposes, the hue is typically scaled to between 0° and
-- 360°. Saturation and brightness are typically scaled to between 0% and 100%.
data HSBK = HSBK
  { hue :: Word16             -- Hue: range 0 to 65535
  , saturation :: Word16      -- Saturation: range 0 to 65535
  , brightness :: Word16      -- Brightness: range 0 to 65535
  , kelvin :: Word16          -- Kelvin: range 2500° (warm) to 9000° (cool)
  } deriving (Eq, Ord, Show)


-- * Light Messages


data LightMessages
  = Get -- ^ 101
  -- Sent by a client to obtain the light state. No payload required. Causes the device to transmit a State message.

  | SetColor Word8 HSBK Word32 -- ^ 102
  -- Sent by a client to change the light state.
  --
  -- Field       | Type
  -- reserved    | unsigned 8-bit integer
  -- color       | HSBK
  -- duration    | unsigned 32-bit integer
  --
  -- The duration is the color transition time in milliseconds.
  --
  -- If the Frame Address res_required field is set to one (1) then the device will transmit a State message.

  | SetWaveform Word8 Word8 HSBK Word32 Float Int16 Word8 -- ^ 103
  -- Apply an effect to the bulb.
  --
  -- This message will be replied to with a State message.
  --
  -- Field       | Type                           | Purpose
  -- reserved    | unsigned 8-bit integer         |
  -- transient   | 8-bit integer as 0 or 1        | Color does not persist.
  -- color       | Hsbk                           | Light end color.
  -- period      | unsigned 32-bit integer        | Duration of a cycle in milliseconds.
  -- cycles      | 32-bit float                   | Number of cycles.
  -- skew_ratio  | signed 16-bit integer          | Waveform Skew, [-32768, 32767] scaled to [0, 1].
  -- waveform    | unsigned 8-bit integer         | Waveform to use for transition.
  --
  -- See Waveforms for more information.

  | SetWaveformOptional Word8 Bool HSBK Word32 Float Int16 Word8 Word8 Word8 Word8 Word8 -- ^ 119
  -- Optionally set effect parameters. Same as SetWaveform but allows some parameters to be set from the current value on device.
  --
  -- This message will be replied to with a State message.
  --
  -- Field           | Type
  -- reserved        | unsigned 8-bit integer
  -- transient       | boolean
  -- color           | Hsbk
  -- period          | unsigned 32-bit integer
  -- cycles          | 32-bit float
  -- skew_ratio      | signed 16-bit integer
  -- waveform        | unsigned 8-bit integer
  -- set_hue         | 8-bit integer as 0 or 1
  -- set_saturation  | 8-bit integer as 0 or 1
  -- set_brightness  | 8-bit integer as 0 or 1
  -- set_kelvin      | 8-bit integer as 0 or 1
  --
  -- See Waveforms for more information.

  | State HSBK Int16 PowerLevel Label Word64 -- ^ 107
  -- Sent by a device to provide the current light state.
  --
  -- Field       | Type
  -- color       | HSBK
  -- reserved    | signed 16-bit integer
  -- power       | unsigned 16-bit integer
  -- label       | string, size: 32 bytes
  -- reserved    | unsigned 64-bit integer
  --
  -- See color, label and power level.

  | GetPower -- ^ 116
  -- Sent by a client to obtain the power level. No payload required. Causes the device to transmit a StatePower message.

  | SetPower PowerLevel Word32 -- ^ 117
  -- Sent by a client to change the light power level.
  --
  -- Field       | Type
  -- level       | unsigned 16-bit integer
  -- duration    | unsigned 32-bit integer
  --
  -- The power level must be either 0 or 65535.
  --
  -- The duration is the power level transition time in milliseconds.
  --
  -- If the Frame Address res_required field is set to one (1) then the device will transmit a StatePower message.

  | StatePower PowerLevel -- ^ 118
  -- Sent by a device to provide the current power level.
  --
  -- Field       | Type
  -- level       | unsigned 16-bit integer

  | GetInfrared -- ^ 120
  -- Gets the current maximum power level of the Infrared channel. If the Frame Address res_required field is set to one (1) then the device will transmit a StateInfrared message.
  --
  -- No payload is required

  | StateInfrared Word16 -- ^ 121
  -- This message is returned from a GetInfrared message. It indicates the current maximum setting for the infrared channel.
  --
  -- Field       | Type
  -- brightness  | unsigned 16-bit integer

  | SetInfrared Word16 -- ^ 122
  -- Send this message to alter the current maximum brightness for the infrared channel.
  --
  -- If the Frame Address res_required field is set to one (1) then the device will transmit a StateInfrared message.
  --
  -- Field       | Type
  -- brightness  | unsigned 16-bit integer
