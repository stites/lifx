module Data.LIFX where

import Prelude
import GHC.Generics
import qualified Data.LIFX.Header as Header

data Message x = Message
  { frame :: Header.Frame
  , frameAddress :: Header.FrameAddress
  , protocolHeader :: Header.ProtocolHeader
  , payload :: x
  } deriving (Eq, Ord, Show, Generic)


