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
module Network.PcapStitch.Header.PayloadType (
PayloadType(PayloadTypePhysical,PayloadTypeLink,PayloadTypeNetwork),
PhysicalPayloadType(..),
LinkPayloadType(..),
NetworkPayloadType(..),
) where
data PhysicalPayloadType = Ethernet | UnknownPhysical deriving (Eq,Ord,Show)
data LinkPayloadType = IPv4 | UnknownLink deriving (Eq,Ord,Show)
data NetworkPayloadType = TCP | UDP | SCTP | ICMP | UnknownNetwork deriving (Eq,Ord,Show)


data PayloadType = PayloadTypePhysical PhysicalPayloadType 
                   | PayloadTypeLink LinkPayloadType
                 | PayloadTypeNetwork NetworkPayloadType
                 deriving (Ord)

instance Show PayloadType where
         showsPrec p (PayloadTypePhysical x) = showsPrec p x
         showsPrec p (PayloadTypeLink x) = showsPrec p x
         showsPrec p (PayloadTypeNetwork x) = showsPrec p x

instance Eq PayloadType where
         (PayloadTypeLink x) == (PayloadTypeLink y) = x == y
         (PayloadTypeNetwork x) == (PayloadTypeNetwork y) = x == y
         _ == _ = False

