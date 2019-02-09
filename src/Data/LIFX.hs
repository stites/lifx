module Data.LIFX where

import Prelude
import GHC.Generics
import qualified Data.LIFX.Header as Header

data Message = Message
  { frame :: Header.Frame
  , frameAddress :: Header.FrameAddress
  , protocolHeader :: Header.ProtocolHeader
  , payload :: Header.Payload
  } deriving (Eq, Ord, Show, Generic)


