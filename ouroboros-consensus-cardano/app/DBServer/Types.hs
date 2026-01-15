module DBServer.Types (HostAddr) where

import Data.Word (Word8)

type HostAddr = (Word8, Word8, Word8, Word8)
