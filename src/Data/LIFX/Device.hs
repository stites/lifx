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

-- | Describes the services exposed by the device.
--
-- When a device is discovered the Service types and IP port are provided.
--
-- All values other than those documented above are reserved for future protocol expansion.
--
-- The LIFX Protocol utilizes UDP/IP for all messages covered by this documentation.
newtype Service
  = UDP -- ^ UDP value is 1
  deriving (Eq, Ord, Show)

