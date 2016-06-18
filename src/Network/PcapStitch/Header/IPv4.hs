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
{-# LANGUAGE ScopedTypeVariables #-}
module Network.PcapStitch.Header.IPv4 (
    IPv4Packet,
    getIPv4Packet,
    protocol,
    ipid,
    fragoff    
) where
import qualified Data.ByteString as B
import Data.Word
import Data.ByteString.Nums.Careless.Int
import Network.PcapStitch.Utilities
import Network.PcapStitch.Header.PayloadType
import Network.PcapStitch.Address
import Network.PcapStitch.Classes
import Network.PcapStitch.HeaderNinja



-- 
--   The IPv4Address Address type gives a useful interface to MAC Addresses
--
-- TODO: There is no need to have the [Word8], it is just here until I can properly pull
-- the IP address out of the Integer.  Ther fact that I need to do this is a bit embarassing.
data IPv4Address = IPv4Address !Integer [Word8]
instance Eq IPv4Address where
         (==) (IPv4Address a _) (IPv4Address b _) = a == b

instance Ord IPv4Address where
         compare (IPv4Address a _) (IPv4Address b _) = compare a b

instance Integral IPv4Address where
  toInteger (IPv4Address b _)= b
instance Enum IPv4Address where
instance Real IPv4Address where
instance Num IPv4Address where 


instance Show IPv4Address where
    showsPrec _ (IPv4Address m w') =
      foldr (\i a -> (shows i).(showString ".").a) (shows (head w)) (reverse (tail w))
      where
       w = reverse w'
        --foldr (\i a -> shows (getWord m i) . showString "." . a) (shows (getWord m 0) ) $ [1,2,3]
        --where
        --  getWord x i = (x `shiftR` (i*8)) .&. 0xff




--ipFromList :: [Word8] -> IPv4Address
--ipFromList  = wordsToInt 4




getIPv4Address :: B.ByteString -> IPv4Address
getIPv4Address x = IPv4Address (strict_unsigned x) (B.unpack x)
    --((ipFromList $ B.unpack x),r)



protoToType :: Word8 -> NetworkPayloadType
protoToType x
            | x == 6 = TCP
            | x == 17 = UDP
            | otherwise = UnknownNetwork



$(makeName "Version")
verf = Field Version getbgword8 RFS4Bit

$(makeName "HeaderLength")
hlenf = Field HeaderLength getbgword8 RFS4Bit

$(makeName "DiffServ")
dscpf = Field DiffServ getbgword8 RFSWord8

$(makeName "TotalLength")
tlenf = Field TotalLength getbgword16 RFSWord16

$(makeName "Ipid")
ipidf = Field Ipid getbgword16 RFSWord16

$(makeName "Flags")
flagsf = Field Flags getbgword8 RFS2Bit

$(makeName "FragOff")
bits = $(arbitraryBits 14)


fof = Field FragOff getbgword16  (RFSNBits bits)
            
$(makeName "TTL")
ttlf = Field TTL getbgword8 RFSWord8

--protof = Field Protocol getbgword8 RFSWord8
$(makeName "Protocol")
protof = Field Protocol (return.protoToType.biggestIntegral) RFSWord8

$(makeName "CheckSum")
chksumf = Field  CheckSum getbgword16 RFSWord16


getaddr = (return . getIPv4Address)
$(makeName "Source")
sourcef = Field Source getaddr RFSWord32

$(makeName "Destination")
destf = Field Destination getaddr RFSWord32

$(makeName "Options")
 


ipv4header =    verf    .++.   hlenf   .++.   dscpf   .++. tlenf
           .++. ipidf   .++.   flagsf  .++.  fof
          .++. ttlf .++. protof .++. chksumf
          .++. sourcef 
          .++. destf
          .++. headerTail

ipv4headertype = recordType $ addOptionsT ipv4header Options


$(wrapInData "IPv4Packet" 'ipv4headertype)


{-
getIPv4Packet :: B.ByteString -> (IPv4Packet,B.ByteString)
getIPv4Packet = 
  (\ (a,b) -> (,) (IPv4Packet a) $ b)
  .  handleHeaderError "Error Parsing IPv4Packet"
  .  getHeader ipv4header
-}

handleError = handleHeaderError "Error Parsing IPv4Packet"



getIPv4Header = getHeader ipv4header

getIPv4Packet :: B.ByteString -> (IPv4Packet,B.ByteString)
getIPv4Packet bs = (IPv4Packet v,snd bstuple)
  where
    tuple = 
      handleError
      .  getIPv4Header $ bs
    rec   = fst tuple
    rest  = snd tuple
    optlen= ((fromIntegral (rec!!!HeaderLength))*4)-20
    bstuple = B.splitAt optlen rest 
    v = addOptions rec Options . fst $ bstuple



source (IPv4Packet x) = x !!! Source  
destination (IPv4Packet x) = x !!! Destination
checksum (IPv4Packet x) = x !!! CheckSum
protocol (IPv4Packet x) = x !!! Protocol
ipid (IPv4Packet x) = x !!! Ipid
fragoff (IPv4Packet x) = x !!! FragOff

instance Show IPv4Packet where
  showsPrec p (IPv4Packet rec) = headerRecordShow p rec


instance  HasSource IPv4Packet where
    getSource x = Address (PayloadTypeLink IPv4) . source $ x
instance  HasDestination IPv4Packet where
    getDestination x = Address (PayloadTypeLink IPv4) . destination $ x


instance HasPayloadType IPv4Packet where
--         getPayloadType x =  PayloadTypeNetwork $ protoToType (protocol x) 
         getPayloadType x =  PayloadTypeNetwork $ protocol x


{-

import Network.PcapStitch.Classes
import Data.Word
import Numeric
import Bits

data  IPv4Packet = IPv4Packet {
                 version :: !Word8,
                 headerlength :: !Word8,
                 diffserv :: !Word8,
                 totallength :: !Word16,
                 ipid :: !Word16,
                 flags :: !Word8,
                 fragoff :: !Word16,
                 ttl :: !Word8,
                 protocol :: !Word8,
                 checksum :: !Word16,
                 destination :: IPv4Address,   
                 source      :: IPv4Address
               }






instance Show IPv4Packet where
   showsPrec p pkt =
       showString "version: " . showsPrec p (version pkt)
           . showString " ihl: " . showsPrec p (headerlength pkt)
           . showString " diffserv: " . showsPrec p (diffserv pkt)
           . showString " length: " . showsPrec p (totallength pkt)
           . showString " id: " . showsPrec p (ipid pkt)
           . showString " flags: " . showsPrec p (flags pkt)
           . showString " fragoff: " . showsPrec p (fragoff pkt)
           . showString " ttl: " . showsPrec p (ttl pkt)
           . showString " protocol: " . showsPrec p (protocol pkt)
           . showString " checksum: " . showsPrec p (checksum pkt)
           . showString " dest: " . showsPrec p (destination pkt)
           . showString " src: " . showsPrec p (source pkt)



instance HasPayloadType IPv4Packet where
         getPayloadType x =  PayloadTypeNetwork $ protoToType (protocol x) 


instance  HasSource IPv4Packet where
    getSource x = Address (PayloadTypeLink IPv4) (source x)

instance  HasDestination IPv4Packet where
    getDestination x = Address (PayloadTypeLink IPv4) (destination x)

{-
instance HasTotalLength IPv4Packet where
  getTotalLength a = fromIntegral $ totallength a



instance Matchable IPv4Packet where
  same a b = and (map  (\ f -> f a b) [fe version,fe ipid,fe source,fe destination])
    where
      fe f a b = (f a) == (f b)
  estTimeToCollision _ = 1 
-}




networklift :: (a -> t )  -> (a,c) -> ( t,c )
networklift f t = (f.fst $ t, snd t)


getIPv4Packet :: B.ByteString ->(IPv4Packet,B.ByteString)
getIPv4Packet str =
    let f = networklift (B.head) 
        g = networklift (\ a -> fst $ B.foldr (\ i (a,c) -> (((fromIntegral i) `shiftL` (c*8)) + a,c+1)) (0,0) a)
        h = getIPv4Address 
        (vandhl,str1) = f (B.splitAt 1 str) 
        plen = (fromIntegral (vandhl .&. 31)) * 4
        (dscp,str2) = f (B.splitAt 1 str1) 
        (tl, str3) = g (B.splitAt 2 str2) 
        (id, str4) = g (B.splitAt 2 str3) 
        (fandl, str5) = g (B.splitAt 2 str4) 
        (ttl, str6) = f (B.splitAt 1 str5) 
        (protocol, str7) = f (B.splitAt 1 str6) 
        (checksum, (str8::B.ByteString)) = g (B.splitAt 2 str7) 
        (src,str9) = h str8 
        (dst,str10) = h str9 
    --Throw away ip options -- NEEDS TO BE HANDLED CORRECTLY!!
        (_,str11) = if plen>20 then B.splitAt (plen-20) str10 else (B.empty,str10) in
    (IPv4Packet (shiftR vandhl 5) (vandhl .&. 31) dscp tl id ((fromIntegral ((shiftR fandl 14)::Word16))::Word8) (fandl .&. 16383) ttl protocol checksum dst src, str11)
-}