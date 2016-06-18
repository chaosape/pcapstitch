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
module Network.PcapStitch.Header.Transport (
    TransportDatagram(..),
    TCPDatagram,
    UDPDatagram,
    getTransportDatagram
) where

import qualified Data.ByteString as B
import Network.PcapStitch.Address
import Network.PcapStitch.Header.PayloadType
import Network.PcapStitch.Classes
import Network.PcapStitch.Header.Header
import Network.PcapStitch.Header.TCP
import Network.PcapStitch.Header.UDP
import Data.Word


data TransportDatagram = TCPTransportDatagram TCPDatagram
                   | UDPTransportDatagram UDPDatagram
                   | UnknownDatagram B.ByteString
                     deriving (Show)



instance HasSource TransportDatagram where
         getSource (TCPTransportDatagram x) = getSource x
         getSource (UDPTransportDatagram x) = getSource x
         getSource _ = NoAddress

instance HasDestination TransportDatagram where
         getDestination (TCPTransportDatagram x) = getDestination x
         getDestination (UDPTransportDatagram x) = getDestination x
         getDestination _ = NoAddress



{-instance Matchable TransportDatagram where
  same (TCPTransportDatagram a) (TCPTransportDatagram b) = same a b
  same (UDPTransportDatagram a) (UDPTransportDatagram b) = same a b
  same _ _ = False
  estTimeToCollision (TCPTransportDatagram a) = estTimeToCollision a 
  estTimeToCollision (UDPTransportDatagram a) = estTimeToCollision a 
  estTimeToCollision _ = -1
import Data.Word
import Numeric
import Bits
import Network.PcapStitch.Classes

-}



--convertToDatagram :: a -> Header TransportDatagram
--convertToDatagram (TCPDatagram x) = Header (TCPTransportDatagram (TCPDatagram x)) 
--convertToDatagram (UDPDatagram x) = Header (UDPTransportDatagram (UDPDatagram x)) 

convert :: (a,B.ByteString)->(a->TransportDatagram)-> (Header TransportDatagram,B.ByteString)
convert (x,y) z = (Header (z x), y) 



getTransportDatagram :: PayloadType->B.ByteString->(Header TransportDatagram,B.ByteString)
getTransportDatagram (PayloadTypeNetwork TCP) x = liftTupleToHeaderTuple $  uncurry (\ a b -> (TCPTransportDatagram a, b)) (getTCPDatagram x)
getTransportDatagram (PayloadTypeNetwork UDP) x = liftTupleToHeaderTuple $  uncurry (\ a b -> (UDPTransportDatagram a, b)) (getUDPDatagram x)
getTransportDatagram _ x 
                       | (B.null x) =  (Empty,B.empty)
                       | otherwise = liftTupleToHeaderTuple $ (_getUnknownDatagram x)

_getUnknownDatagram :: B.ByteString -> (TransportDatagram,B.ByteString)
_getUnknownDatagram x = (UnknownDatagram x, B.empty)



