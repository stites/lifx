-------------------------------------------------------------------------------
-- |
-- Module    :  Data.LIFX.Device
-- Copyright :  (c) Sam Stites 2017
-- License   :  BSD-3-Clause
-- Maintainer:  fnz@fgvgrf.vb (cipher:rot13)
-- Stability :  experimental
-- Portability: non-portable
--
-- Device messages control and acquire the state of a device, independent of its
-- specific type. The state is composed of the label, port, power level, time,
-- firmware version and microcontroller statistics.
--
-- These messages are common to all LIFX devices, which may also implement
-- device specific messages, such as Light Messages.
--
-- The Protocol header message type field value (decimal) appears after each
-- message name in this document. Each section below describes the payload that
-- is appended to the message header. Some messages do not require a payload.
-------------------------------------------------------------------------------
module Data.LIFX.Device where

import Prelude

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Data.ByteString
import GHC.Int
import GHC.Generics
import GHC.Word

-- *  Enumerated Types

-- | Describes the services exposed by the device.
--
-- When a device is discovered the Service types and IP port are provided.
--
-- All values other than those documented above are reserved for future protocol expansion.
--
-- The LIFX Protocol utilizes UDP/IP for all messages covered by this documentation.
data Service
  = UDP             -- ^ UDP value is 1, it should be represented as an unsigned 8-bit integer
  deriving (Eq, Ord, Show)

instance Serialise Service where
  encode :: Service -> Encoding
  encode UDP = encodeBool True

  decode :: Decoder s Service
  decode =
    decodeBool >>= \case
      True -> pure UDP
      False -> fail "Error: Expected to decode a single bit-encoded 1 representing UDP. Found 0."

serviceValue :: Num i => Service -> i
serviceValue UDP = 1

-- *  Message Field Data Types

-- | For user interfaces purposes, each device can be identified by their label as well as a group and a location.
--
-- The label string is a fixed-length field, which is not null-terminated.
--
-- If you are setting a label/group/location for your light, it is highly recommended you store the label as a utf-8 string for compatibility with existing clients.
newtype Label = Label String
  deriving (Eq, Ord, Show, Generic)


instance Serialise Label

-- The IP port number used by a LIFX device for a specific 'Service'.
--
-- Whilst LIFX devices usually listen to port 56700, it is strongly recommended that devices are discovered and the provided port number is used.
--
-- As of LIFX Protocol V2, LIFX devices sending unicast message responses will reply to whichever port number the client has bound. See message header frame source identifier regarding the conditions under which unicast messages are sent.
--
-- Prior to LIFX Protocol V2, i.e the Original LIFX A21 lightbulb running firmware version 1.x, LIFX devices send broadcast message responses to port 56700.
--
-- To get the best compatibility across both newer and older LIFX devices, clients should bind to UDP port 56700.
newtype Port = Port Word32
  deriving (Eq, Ord, Show, Generic)

instance Serialise Port

-- | The power level can be either standby (0) or enabled (65535).
-- Currently, only the values 0 and 65535 are supported
data PowerLevel
  = Standby
  | Enabled
  deriving (Eq, Ord, Show, Generic)

instance Serialise PowerLevel where
  encode = \case
    Standby -> encodeWord16 0
    Enabled -> encodeWord16 65535

  decode =
    decodeWord16 >>= \case
      0     -> pure Standby
      65535 -> pure Enabled
      x -> fail $ "Error: Powerlevel only supports the values 0 (standby) and 65535 (enabled). Found: " ++ show x

-- | All time values have a precision of nanoseconds. When an absolute time value is provided, then it is the number of nanoseconds since the epoch, i.e Thursday 1st January 1970 00:00:00.
newtype Time = Time Word64
  deriving (Eq, Ord, Show, Generic)

instance Serialise Time

-- *  Device Messages

data DeviceMessages
  = GetService -- ^ 2
  -- Sent by a client to acquire responses from all devices on the
  -- local network. No payload is required. Causes the devices to transmit
  -- a 'StateService' message.
  --
  -- When using this message the Frame tagged field must be set to one (1).

  | StateService Service Port  -- ^ 3
  -- Response to GetService message.
  --
  -- Provides the device Service and port. If the Service is temporarily
  -- unavailable, then the port value will be 0.
  --
  -- Field0: service - unsigned 8-bit integer, maps to Service
  -- Field0: port    - unsigned 32-bit integer

  | GetHostInfo  -- ^ 12
  -- Get Host MCU information. No payload is required. Causes the device to transmit a StateHostInfo message.

  | StateHostInfo Float Word32 Word32 Int16 -- ^ 13
  -- Response to GetHostInfo message.
  --
  -- Provides host MCU information.
  --     signal: radio receive signal strength in milliWatts
  --     tx: bytes transmitted since power on
  --     rx: bytes received since power on
  --
  -- Field    | Type
  -- signal   | 32-bit float
  -- tx       | unsigned 32-bit integer
  -- rx       | unsigned 32-bit integer
  -- reserved | signed 16-bit integer

  | GetHostFirmware -- ^ 14
  -- Gets Host MCU firmware information. No payload is required. Causes the device to transmit a StateHostFirmware message.

  | StateHostFirmware Word64 Word64 Word32 -- ^ 15
  -- Response to GetHostFirmware message.
  --
  -- Provides host firmware information.
  --
  --     build: firmware build time (absolute time in nanoseconds since epoch)
  --     version: firmware version
  --
  -- Field          | Type
  -- build          | unsigned 64-bit integer
  -- reserved       | unsigned 64-bit integer
  -- version        | unsigned 32-bit integer

  | GetWifiInfo -- ^ 16
  -- Get Wifi subsystem information. No payload is required. Causes the device to transmit a StateWifiInfo message.

  | StateWifiInfo Float Word32 Word32 Int16  -- ^ 17
  -- Response to GetWifiInfo message.
  --
  -- Provides Wifi subsystem information.
  --
  --     signal: radio receive signal strength in mw
  --     tx: bytes transmitted since power on
  --     rx: bytes received since power on
  --
  -- Field       | Type
  -- signal      | 32-bit float
  -- tx          | unsigned 32-bit integer
  -- rx          | unsigned 32-bit integer
  -- reserved    | signed 16-bit integer

  | GetWifiFirmware Word64 Word64 Word32 -- ^ 18
  -- Get Wifi subsystem firmware. No payload is required. Causes the device to transmit a StateWifiFirmware message.
  -- StateWifiFirmware - 19
  --
  -- Response to GetWifiFirmware message.
  --
  -- Provides Wifi subsystem information.
  --
  --     build: firmware build time (absolute time in nanoseconds since epoch)
  --     version: firmware version
  --
  -- Field     | Type
  -- build     | unsigned 64-bit integer
  -- reserved  | unsigned 64-bit integer
  -- version   | unsigned 32-bit integer

  | GetPower -- ^ 20
  -- Get device power level. No payload is required. Causes the device to transmit a StatePower message.

  | SetPower PowerLevel -- ^ 21
  -- Set device power level.
  --
  -- Zero implies standby and non-zero sets a corresponding power draw level. Currently only 0 and 65535 are supported.
  -- Field          | Type
  -- level          | unsigned 16-bit integer

  | StatePower PowerLevel -- ^ 22
  -- Response to GetPower message.
  --
  -- Provides device power level.
  --
  -- Field          | Type
  -- level          | unsigned 16-bit integer

  | GetLabel -- ^ 23
  -- Get device label. No payload is required. Causes the device to transmit a StateLabel message.

  | SetLabel ByteString -- ^ 24
  -- Set the device label text.
  --
  -- Field          | Type
  -- label          | string, size: 32 bytes

  | StateLabel ByteString -- ^ 25
  -- Response to GetLabel message.
  --
  -- Provides device label.
  --
  -- Field          | Type
  -- label          | string, size: 32 bytes

  | GetVersion -- ^ 32
  -- Get the hardware version. No payload is required. Causes the device to transmit a StateVersion message.

  | StateVersion Word32 Word32 Word32 -- ^ 33
  -- Response to GetVersion message.
  --
  -- Provides the hardware version of the device. See LIFX Products for how to interpret the vendor and product ID fields.
  --
  --     vendor: vendor ID
  --     product: product ID
  --     version: hardware version
  --
  -- Field          | Type
  -- vendor         | unsigned 32-bit integer
  -- product        | unsigned 32-bit integer
  -- version        | unsigned 32-bit integer

  | GetInfo -- ^ 34
  -- Get run-time information. No payload is required. Causes the device to transmit a StateInfo message.

  | StateInfo Time Time Time -- ^ 35
  -- Response to GetInfo message.
  --
  -- Provides run-time information of device.
  --
  --     time: current time (absolute time in nanoseconds since epoch)
  --     uptime: time since last power on (relative time in nanoseconds)
  --     downtime: last power off period, 5 second accuracy (in nanoseconds)
  --
  -- Field       | Type
  -- time        | unsigned 64-bit integer
  -- uptime      | unsigned 64-bit integer
  -- downtime    | unsigned 64-bit integer

  | Acknowledgement -- ^ 45
  -- Response to any message sent with _ack_required_ set to 1. See message header frame address.

  | GetLocation -- ^ 48
  -- Ask the bulb to return its location information.
  -- No payload is required.
  -- Causes the device to transmit a StateLocation message.

  | SetLocation ByteString Label Time -- ^ 49
  -- Set the device location.
  --
  -- Applications wishing to change either the label or location attributes MUST
  -- set the updated_at field to the current timestamp and send the message to
  -- all applicable devices that are currently online. This is because when
  -- reading these values the applications will consider unique location
  -- fields to be a location identifier, and the label on the bulb with the
  -- highest updated_at field for that location will be used. Applications
  -- SHOULD attempt to correct any labels that are out of date when found.
  --
  --It is recommended to set the response_required flag on the message header
  --to speed up updating the cloud account.
  --
  -- When creating a new location generate the location field with random data.
  --
  --     location: guid byte array.
  --     label: text label for location.
  --     updated_at: UTC timestamp of last label update in nanoseconds.
  --
  -- Field          | Type
  -- location       | byte array, size: 16
  -- label          | string, size: 32
  -- updated_at     | integer, size: 64

  | StateLocation ByteString Label Time -- ^ 50
  -- Device location.
  --
  -- Field          | Type
  -- location       | byte array, size: 16 bytes
  -- label          | string, size: 32 bytes
  -- updated_at     | unsigned 64-bit integer

  | GetGroup -- ^ 51
  -- Ask the bulb to return its group membership information.
  -- No payload is required.
  -- Causes the device to transmit a StateGroup message.

  | SetGroup ByteString Label Time -- ^ 52
  -- Set the device group.
  --
  -- Applications wishing to change either the label or group attributes MUST
  -- set the updated_at field to the current timestamp and send the message to
  -- all applicable devices that are currently online. This is because when
  -- reading these values the applications will consider unique group fields
  -- to be a group identifier, and the label on the bulb with the highest
  -- updated_at field for that group will be used. Applications SHOULD attempt
  -- to correct any labels that are out of date when found.
  --
  -- It is recommended to set the response_required flag on the message header
  -- to speed up updating the cloud account.
  --
  -- When creating a new group generate the group field with random data.
  --
  --     group: guid byte array.
  --     label: text label for group.
  --     updated_at: UTC timestamp of last label update in nanoseconds.
  --
  -- Field          | Type
  -- group          | byte array, size: 16
  -- label          | string, size: 32
  -- updated_at     | integer, size: 64

  | StateGroup ByteString Label Time -- ^ 53
  -- Device group.
  --
  -- Field          | Type
  -- group          | byte array, size: 16 bytes
  -- label          | string, size: 32 bytes
  -- updated_at     | unsigned 64-bit integer

  | EchoRequest ByteString -- ^ 58
  -- Request an arbitrary payload be echoed back. Causes the device to transmit an EchoResponse message.
  --
  -- Field          | Type
  -- payload        | byte array, size: 64 bytes

  | EchoResponse ByteString -- ^ 59
  -- Response to EchoRequest message.
  --
  -- Echo response with payload sent in the EchoRequest.
  -- Field          | Type
  -- payload        | byte array, size: 64 bytes
