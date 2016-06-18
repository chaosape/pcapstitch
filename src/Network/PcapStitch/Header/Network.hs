--------------------------------------------------------------------------------
-- | 
--  Copyright: Dan DaCosta 2010, Chaosape LLC
--  License: GPLv3
--  Maintainer: chaosape@chaosape.com
--  Stability: expermental
--  Portability: non-portable
--
-- Description: TBD
--------------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Network.PcapStitch.Header.Network (
    NetworkPacket(..),
    IPv4Packet(..),
    getNetworkPacket,
    unwrapNetworkPacketAsIPv4Packet
) where
import qualified Data.ByteString as B
import Network.PcapStitch.Address
import Network.PcapStitch.Classes
import Network.PcapStitch.Header.PayloadType
import Network.PcapStitch.Header.Header
import Network.PcapStitch.Header.IPv4



data NetworkPacket =
  NetworkIPv4Packet IPv4Packet
  | NetworkIPv6Packet
  | UnknownPacket B.ByteString

unwrapNetworkPacketAsIPv4Packet (NetworkIPv4Packet p) = p
unwrapNetworkPacketAsIPv4Packet _ = error "unwrapNetworkPacketAsIPv4Packet: Not an IPv4 Packet!!!" 


instance Show NetworkPacket where
         showsPrec p (NetworkIPv4Packet x) = showsPrec p x
         showsPrec p (UnknownPacket x) = showsPrec p x
         showsPrec p NetworkIPv6Packet = showString "NetworkIPv6Packet"




instance HasPayloadType NetworkPacket where
    getPayloadType (NetworkIPv4Packet x) = getPayloadType x 
    getPayloadType NetworkIPv6Packet = PayloadTypeNetwork UnknownNetwork
    getPayloadType (UnknownPacket x) = PayloadTypeNetwork UnknownNetwork


instance HasSource NetworkPacket where
         getSource (NetworkIPv4Packet x) = getSource x
         getSource x = NoAddress  --BadAddress $ PayloadTypeNetwork UnknownNetwork

instance HasDestination NetworkPacket where
         getDestination (NetworkIPv4Packet x) = getDestination x
         getDestination x = NoAddress --BadAddress $ PayloadTypeNetwork UnknownNetwork

{-
instance HasTotalLength NetworkPacket where
  getTotalLength (NetworkIPv4Packet a) = getTotalLength a
  getTotalLength _ = error "HasTotalLength NetworkPacket: unhandled pattern"

instance Matchable NetworkPacket where
  same (NetworkIPv4Packet a) (NetworkIPv4Packet b) = same a b
  same _ _ = False
  estTimeToCollision (NetworkIPv4Packet a) = estTimeToCollision a 
  estTimeToCollision _ = -1
-}




getNetworkPacket :: PayloadType->B.ByteString->(Header NetworkPacket,B.ByteString)
-- | 
-- This definition will require more to split between IPv4 and IPv6 in the 
-- future.  I may actually want to split the IP link payload type into two
-- different types; however this unnecessary because the first field in the both
-- IP packets specify what version they are.
--
getNetworkPacket (PayloadTypeLink IPv4) x | (B.null x) = (Empty,B.empty)
                      | otherwise =  liftTupleToHeaderTuple $  uncurry (\ a b -> (NetworkIPv4Packet a, b)) $ getIPv4Packet x
getNetworkPacket _ x  = liftTupleToHeaderTuple $ _getUnknownPacket x


_getUnknownPacket :: B.ByteString -> (NetworkPacket,B.ByteString)
_getUnknownPacket x =  (UnknownPacket x, B.empty)

