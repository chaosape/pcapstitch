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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.PcapStitch.Header.Ethernet (
    MACAddress,
    EthernetFrame,
    getEthernetFrame,
) where
import Data.Word
import Numeric
import Bits
import qualified Data.ByteString as B
import Network.PcapStitch.Header.PayloadType
import Network.PcapStitch.Address
import Network.PcapStitch.Classes
import Network.PcapStitch.Utilities
import Network.PcapStitch.HeaderNinja
import Control.Monad




newtype EthernetType = EthernetType Word16
    deriving (Num, Eq)



etherTypeName e
    | e == 0x0800   = "IPv4"
    | otherwise     = "Unknown"

instance Show EthernetType where
    showsPrec _ (EthernetType e) =
        showString $ tail $ showHex ((16 :: Int)^(4 :: Int)+ 0x0800) $ " " ++ etherTypeName e

$(makeName "Source")
sourcef = Field Source getbgword64 RFSWord48
$(makeName "Destination")
destf = Field Destination getbgword64 RFSWord48
$(makeName "Protocol")
protof = Field Protocol (liftM EthernetType . getbgword16) RFSWord16
ethheader =  destf .++. sourcef .++. protof .++. headerTail
ethheadertype = recordType ethheader
$(wrapInData "EthernetFrame" 'ethheadertype)
getEthHeader = getHeader ethheader
handleError = handleHeaderError "Error Parsing EthernetFrame"
getEthernetFrame :: B.ByteString -> (EthernetFrame,B.ByteString)
getEthernetFrame bs = (EthernetFrame rec,rest)
  where
    tuple = handleError . getEthHeader $ bs
    rec   = fst tuple
    rest  = snd tuple

source (EthernetFrame x) = x !!! Source  
destination (EthernetFrame x) = x !!! Destination
payloadtype (EthernetFrame x) = x !!! Protocol





instance Show EthernetFrame where
  showsPrec p (EthernetFrame rec) = headerRecordShow p rec




ethernetType :: EthernetFrame -> LinkPayloadType
ethernetType a
   | payloadtype a == 0x0800 = IPv4
   | otherwise               = UnknownLink



instance HasPayloadType EthernetFrame where
    getPayloadType x = PayloadTypeLink (ethernetType x) 


instance  HasSource EthernetFrame where
    getSource x = Address (PayloadTypePhysical Ethernet) (MACAddress . source $ x)

instance  HasDestination EthernetFrame where
    getDestination x = Address (PayloadTypePhysical Ethernet) (MACAddress . destination $ x)



-- 
--   The MACAddress type gives a useful interface to MAC Addresses
--

newtype MACAddress = MACAddress Word64
    deriving (Eq, Ord, Bits, Num, Integral, Enum, Real)

instance Show MACAddress where
    showsPrec _ (MACAddress m) =
        foldr (\i a -> showsHexByte (getWord m i)  ":" . a) (showsHexByte (getWord m 0)  "") $ [5,4..1]
        where
            getWord x i = (x `shiftR` (i*8)) .&. 0xff
--            showsHexByte :: forall t. (Integral t) => t -> String -> String -> String
            showsHexByte x a = showString $ tail $ showHex (16^(2 :: Int)+x) a

macFromList :: [Word8] -> MACAddress
macFromList  = wordsToInt 6


-- 
--   The EthernetType type is designed to show the type of payload in an Ethernet packet
--



{-
-- 
--   The EthPkt type defines an Ethernet II packet with another Packet payload
--

data EthernetFrame = EthernetFrame {
                 destination :: MACAddress,    -- ^ destination MAC address
                 source      :: MACAddress,    -- ^ source MAC address
                 payloadtype        :: EthernetType    -- ^ payload type
               }

instance Show EthernetFrame where
   showsPrec p pkt =
       showString "Ethernet II dest: " . showsPrec p (destination pkt)
           . showString " src: " . showsPrec p (source pkt)
           . showString " type: " . showsPrec p (payloadtype pkt)


getEthernetFrame :: B.ByteString->(EthernetFrame,B.ByteString)
getEthernetFrame x =
    let f  = biggestIntegral  
        g  = biggestIntegral
        (dst,remain) = B.splitAt 6 x 
        (src,remain') = B.splitAt 6 remain 
        (typ,remain'') = B.splitAt 2 remain' in
    (EthernetFrame (f dst) (f src) (EthernetType (g typ)), remain'')
-}