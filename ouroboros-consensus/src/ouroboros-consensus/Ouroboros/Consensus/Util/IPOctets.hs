module Ouroboros.Consensus.Util.IPOctets (IPOctets, fromIPTuple, parseIPOctets, printIP, toIPTuple, toSockAddrV4) where

import qualified Data.IP as IP
import Data.List (intercalate)
import Data.Word (Word8)
import qualified Network.Socket as Socket
import Text.Read (readMaybe)

newtype IPOctets = IPOctets {unIPOctets :: (Word8, Word8, Word8, Word8)}
  deriving (Show, Eq)

fromIPTuple :: (Word8, Word8, Word8, Word8) -> IPOctets
fromIPTuple tuple = IPOctets tuple

toIPTuple :: IPOctets -> (Word8, Word8, Word8, Word8)
toIPTuple (IPOctets tuple) = tuple

toSockAddrV4 :: IPOctets -> Socket.PortNumber -> Socket.SockAddr
toSockAddrV4 (IPOctets tup) port = Socket.SockAddrInet port (Socket.tupleToHostAddress tup)

parseIPOctets :: String -> Maybe IPOctets
parseIPOctets s = IPOctets . Socket.hostAddressToTuple . IP.toHostAddress <$> (readMaybe s)

printIP :: IPOctets -> String
printIP (IPOctets (a, b, c, d)) = intercalate "." $ map show [a, b, c, d]