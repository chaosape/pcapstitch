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
module Network.PcapStitch.Header.TCP (
    TCPDatagram,
    sequenceN,
    getTCPDatagram,
    acknowledgementN,
    checksum,
    offset,
) where
import qualified Data.ByteString as B
import Network.PcapStitch.Utilities
import Network.PcapStitch.Address
import Network.PcapStitch.Classes
import Network.PcapStitch.Header.PayloadType
import Network.PcapStitch.HeaderNinja






$(makeName "Source")
sourcef = Field Source getbgword16 RFSWord16

$(makeName "Destination")
destf = Field Destination getbgword16 RFSWord16

$(makeName "Sequence")
seqf = Field Sequence getbgword32 RFSWord32

$(makeName "Acknowledgement")
ackf = Field Acknowledgement getbgword32 RFSWord32

$(makeName "Offset")
offf = Field Offset getbgword8 RFS4Bit

$(makeName "Resv")
resvf = Field Resv getbgword8 RFS4Bit

$(makeName "Flags")
flagsf = Field Flags getbgword8 RFSWord8

$(makeName "Window")
winf = Field Window getbgword16 RFSWord16

$(makeName "CheckSum")
chksumf = Field  CheckSum getbgword16 RFSWord16

$(makeName "Urgent")
urgf = Field  Urgent getbgword16 RFSWord16

$(makeName "Options")


$(makeName "Test")
testf = Field Test getbgword8 RFS4Bit

         
tcpheader =    sourcef .++. destf 
          .++. seqf 
          .++. ackf 
          .++. offf .++. resvf .++. flagsf .++. winf 
          .++. chksumf .++. urgf
          .++. headerTail

{-
tcpheaderCheck =    sourcef .++. destf 
          .++. seqf 
          .++. ackf 
          .++. offf .++. resvf .++. flagsf .++. winf 
          .++. chksumf .++. urgf .++. testf
          .++. headerTail
tcpheadertypestatic =  recordType $ addOptionsT tcpheaderCheck  Options
-}

tcpheadertype = recordType $ addOptionsT tcpheader Options


$(wrapInData "TCPDatagram" 'tcpheadertype)

handleError = handleHeaderError "Error Parsing TCPDatagram"


getTCPHeader = getHeader tcpheader

getTCPDatagram :: B.ByteString -> (TCPDatagram,B.ByteString)
getTCPDatagram bs = (tcpd,snd bstuple)
  where
    tuple = 
      handleError
      .  getTCPHeader $ bs
    rec   = fst tuple
    rest  = snd tuple
    optlen= ((fromIntegral (rec!!!Offset))*4)-20
    bstuple = B.splitAt optlen rest 
    tcpd  = TCPDatagram . addOptions rec Options . fst $ bstuple
     
   
 
    

source (TCPDatagram x) = x !!! Source  
destination (TCPDatagram x) = x !!! Destination
checksum (TCPDatagram x) = x !!! CheckSum
offset (TCPDatagram x) = x !!! Offset
sequenceN (TCPDatagram x) = x !!! Sequence
acknowledgementN (TCPDatagram x) = x !!! Acknowledgement

instance Show TCPDatagram where
  showsPrec p (TCPDatagram rec) = headerRecordShow p rec


instance  HasSource TCPDatagram where
    getSource x = Address (PayloadTypeNetwork TCP) (source x)
instance  HasDestination TCPDatagram where
    getDestination x = Address (PayloadTypeNetwork TCP)  (destination x)


{-

import Network.Socket
import Debug.Trace
import Data.Word
import Numeric
import Bits
import Control.Monad


-- 
--
data TCPDatagram = TCPDatagram 
 { source            :: !Word16
 , destination       :: !Word16
 , sequenceN         :: !Word
 , acknowledgementN  :: !Word
 , offset            :: !Word8
 , resv              :: !Word8
 , cwr               :: !Bool
 , ece               :: !Bool
 , urg               :: !Bool
 , ack               :: !Bool
 , psh               :: !Bool
 , rst               :: !Bool
 , syn               :: !Bool
 , fin               :: !Bool
 , win               :: !Word16
 , checksum          :: !Word16
 , urgent            :: !Word16
 , options           :: B.ByteString
 }

ntohs :: [Word8] -> Word16
ntohs i = case (fromIntegral (wordsToWord16 i))::PortNumber of PortNum p -> p 

instance Show TCPDatagram where
   showsPrec p pkt =
       showString "source: " . showsPrec p (source pkt)
           . showString "  destination: " . showsPrec p (destination pkt)
           . showString "  sequence: " . showsPrec p (sequenceN pkt)
           . showString "  acknowledgement: " . showsPrec p (acknowledgementN pkt)
           . showString "  offset: " . showsPrec p (offset pkt)
           . showString "  resvered: " . showsPrec p (resv pkt)
           . showString "  CWR: " . showsPrec p (cwr pkt)
           . showString "  ECE: " . showsPrec p (ece pkt)
           . showString "  URG: " . showsPrec p (urg pkt)
           . showString "  ACK: " . showsPrec p (ack pkt)
           . showString "  PSH: " . showsPrec p (psh pkt)
           . showString "  RST: " . showsPrec p (rst pkt)
           . showString "  SYN: " . showsPrec p (syn pkt)
           . showString "  FIN: " . showsPrec p (fin pkt)
           . showString "  window: " . showsPrec p (win pkt)
           . showString "  checksum: " . showsPrec p (checksum pkt)
           . showString "  urgent: " . showsPrec p (urgent pkt)



instance  HasSource TCPDatagram where
    getSource x = Address (PayloadTypeNetwork TCP) (source x)

instance  HasDestination TCPDatagram where
    getDestination x = Address (PayloadTypeNetwork TCP)  (destination x)


{-
instance Matchable TCPDatagram where
  same a b = and (map  (\ f -> f a b) [fe source,fe destination,fe sequenceN,fe acknowledgementN,fe ack,fe psh,fe syn,fe fin,fe rst])
    where
      fe f a b = (f a) == (f b)
  estTimeToCollision _ = 85
-}


data Flags = CWR | ECE | URG | ACK | PSH | RST | SYN | FIN


word8ToBool :: Word8 -> Bool
word8ToBool v
            | v == 0 = False
            | otherwise = True

getFlag :: Flags->Word8-> Bool
getFlag FIN v = word8ToBool (v .&. 128) 
getFlag SYN v = word8ToBool (v .&. 64) 
getFlag RST v = word8ToBool (v .&. 32) 
getFlag ACK v = word8ToBool (v .&. 16) 
getFlag PSH v = word8ToBool (v .&. 8) 
getFlag URG v = word8ToBool (v .&. 4) 
getFlag ECE v = word8ToBool (v .&. 2) 
getFlag CWR v = word8ToBool (v .&. 1) 


word32toInt :: Word32 -> Int
word32toInt = fromIntegral

transportlift :: (a -> t )  -> (a,c) -> ( t,c )
transportlift f t = (f.fst $ t,snd t) 

debug :: (Show a)=>a -> a
--debug x = trace (show x) x 
debug x = x 

-- | TODO - Pull out TCP options currently they are going to end up in 
getTCPDatagram :: B.ByteString -> (TCPDatagram,B.ByteString)
getTCPDatagram str = 
    let f = transportlift (B.head) 
        g = transportlift (biggestIntegral )
        g' = transportlift (biggestIntegral ) 
        h = transportlift (biggestIntegral ) 
        (source, str1) = g' (B.splitAt 2 str) 
        (destination, str2) = g' (B.splitAt 2 str1)
        (seq', str3) = h (B.splitAt 4 str2)
        (ack, str4) = h (B.splitAt 4 str3) 
        (oandr, str5) = f (B.splitAt 1 str4)
        (flags, str6) = f (B.splitAt 1 str5)
        (wind, str7) = g (B.splitAt 2 str6)
        (cs, str8) = g (B.splitAt 2 str7) 
        (up, str9) = g (B.splitAt 2 str8) 
        offset' = (shiftR oandr 5)
        optlen = 40 - offset'*4
        (options, str10) = B.splitAt (fromIntegral optlen) str9
        in
    (,) (TCPDatagram 
         (debug source) 
         (debug destination) 
         seq' 
         ack 
         offset' 
         (oandr .&. 31) 
         (getFlag CWR flags)  
         (getFlag ECE flags)  
         (getFlag URG flags)  
         (getFlag ACK flags)  
         (getFlag PSH flags)  
         (getFlag RST flags)  
         (getFlag SYN flags)  
         (getFlag FIN flags) 
         wind 
         cs 
         up
         options) 
        $ str10
-}