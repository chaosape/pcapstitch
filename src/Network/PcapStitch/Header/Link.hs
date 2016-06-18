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
module Network.PcapStitch.Header.Link (
    LinkFrame(..),
    LinkPayloadType,
    getLinkFrame
) where
import qualified Data.ByteString as B
import Network.PcapStitch.Header.PayloadType
import Network.PcapStitch.Header.Header
import Network.PcapStitch.Header.Ethernet
import Network.PcapStitch.Address
import Network.PcapStitch.Classes

data LinkFrame = LinkEthernetFrame EthernetFrame
             | UnknownFrame B.ByteString


instance HasPayloadType LinkFrame where
    getPayloadType (LinkEthernetFrame x) = getPayloadType x 
    getPayloadType (UnknownFrame x) = PayloadTypeLink UnknownLink



instance Show LinkFrame where 
    showsPrec p (LinkEthernetFrame e) = showsPrec p e
    showsPrec p (UnknownFrame e) = showsPrec p e               


getLinkFrame :: PayloadType->B.ByteString->(Header LinkFrame,B.ByteString)
getLinkFrame  (PayloadTypePhysical Ethernet) x = liftTupleToHeaderTuple $ uncurry (\ a b -> (LinkEthernetFrame a, b)) (getEthernetFrame x)
getLinkFrame  _ x = liftTupleToHeaderTuple $ _getUnknownFrame x

instance HasSource LinkFrame where
         getSource (LinkEthernetFrame x) = getSource x
         getSource x = BadAddress $ PayloadTypeLink UnknownLink

instance HasDestination LinkFrame where
         getDestination (LinkEthernetFrame x) = getDestination x
         getDestination x = BadAddress $ PayloadTypeLink UnknownLink


_getUnknownFrame :: B.ByteString -> (LinkFrame,B.ByteString)
_getUnknownFrame x =  (UnknownFrame x, B.empty)
