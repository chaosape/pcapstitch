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
{-# LANGUAGE TemplateHaskell #-}
module Network.PcapStitch.Header.UDP (
    UDPDatagram,
    getUDPDatagram,
    checksum
) where
import qualified Data.ByteString as B
import Network.PcapStitch.Utilities
import Network.PcapStitch.Address
import Network.PcapStitch.Classes
import Network.PcapStitch.Header.PayloadType
import Network.PcapStitch.HeaderNinja
import Data.Binary.Strict.BitGet
import Data.Word

getword16 :: B.ByteString -> BitGet Word16
getword16 = return . biggestIntegral


$(makeName "Source")
sourcef = Field Source getword16 RFSWord16
$(makeName "Destination")
destf = Field Destination getword16 RFSWord16
$(makeName "Len")
lenf = Field Len getword16 RFSWord16
$(makeName "CheckSum")
chksumf = Field  CheckSum getword16 RFSWord16


udpheader = sourcef .++. destf .++. lenf .++. chksumf .++. headerTail

udpheadertype = recordType udpheader


$(wrapInData "UDPDatagram" 'udpheadertype)

getUDPDatagram :: B.ByteString -> (UDPDatagram,B.ByteString)
getUDPDatagram = 
  (\ (a,b) -> (UDPDatagram $ a,b))
  .  handleHeaderError "Error Parsing UDPDatagram"
  .  getHeader udpheader

source (UDPDatagram x) = x !!! Source  
destination (UDPDatagram x) = x !!! Destination
len (UDPDatagram x) = x !!! Len  
checksum (UDPDatagram x) = x !!! CheckSum

instance Show UDPDatagram where
  showsPrec p (UDPDatagram rec) = headerRecordShow p rec


instance  HasSource UDPDatagram where
    getSource x = Address (PayloadTypeNetwork UDP) (source x)
instance  HasDestination UDPDatagram where
    getDestination x = Address (PayloadTypeNetwork UDP)  (destination x)



