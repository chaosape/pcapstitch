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
module Network.PcapStitch.Message where
import qualified Data.ByteString as B
import Data.ByteString.Internal(w2c)
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.HashTable
import Data.Digest.Pure.SHA
import qualified Data.Map as M
import Network.PcapStitch.PcapSources
import Network.PcapStitch.Timestamp
import Network.PcapStitch.ManageablePacketEvent
import Network.PcapStitch.Address
import Network.PcapStitch.Classes
import Network.PcapStitch.Header.PayloadType
import Network.PcapStitch.Header.Header
import Network.PcapStitch.Header.Link
import Network.PcapStitch.Header.Network
import Network.PcapStitch.Header.Transport
import qualified Network.PcapStitch.Header.TCP as TCP
import qualified Network.PcapStitch.Header.UDP as UDP
import qualified Network.PcapStitch.Header.IPv4 as IPv4
import Network.PcapStitch.PacketEquivalencyFunction
import Data.List
--import Data.List.Stream 
--import Prelude hiding ((++),(!!),foldl,length,splitAt,span,head,map,null,and,last)

type Triple = (Timestamp,PcapFile,Integer)
fstT :: Triple -> Timestamp
fstT (x,_,_) = x
sndT :: Triple -> PcapFile
sndT (_,x,_) = x
trdT :: Triple -> Integer
trdT (_,_,x) = x


type ApplicationData = BL.ByteString

data Message = Message
             {
               pkteqfun :: [PacketEquivalencyFunction],
               huuid :: {-# UNPACK #-} !Int32,
               fid :: {-# UNPACK #-} !Integer,
               wirelength :: {-# UNPACK #-} !Integer, 
               physical :: {-# UNPACK #-} !PayloadType,
               link ::  Header LinkFrame,
               network :: Header NetworkPacket,
               transport ::Header TransportDatagram,
               application :: ApplicationData,
               matches :: [Triple]
             } deriving (Show)



uniqueID :: [PacketEquivalencyFunction] -> Message -> Maybe Int32
uniqueID pef m = uniqueID' 
               (uPhy $ physical m) 
               (uLink $ getPayloadType $ link m, runHeader $ link m) 
               (uNet $ getPayloadType $ network m, runHeader $ network m) 
               (runHeader $ transport m)
               (application m)
  where
    uniqueID' :: PhysicalPayloadType->(LinkPayloadType,LinkFrame)->(NetworkPayloadType,NetworkPacket)-> TransportDatagram -> ApplicationData -> Maybe Int32
    uniqueID' Ethernet (IPv4,LinkEthernetFrame _) (TCP,NetworkIPv4Packet _) (TCPTransportDatagram _) _ = Just . hashMessage pef $ m
    uniqueID' Ethernet (IPv4,LinkEthernetFrame _) (UDP,NetworkIPv4Packet _) (UDPTransportDatagram _) _ = Just . hashMessage pef $ m
    uniqueID' _ _ _ _ _ = Nothing

uPhy a   = case a of PayloadTypePhysical b -> b
uLink a  = case a of PayloadTypeLink b -> b
uNet a   = case a of PayloadTypeNetwork b -> b
uIPv4 a  = case a of NetworkIPv4Packet b -> b



pefToShowS :: PacketEquivalencyFunction -> (Message ->ShowS)
pefToShowS AppFunction = showString . map w2c .  BL.unpack . application
pefToShowS FileMatchAware = (\ _ -> id) 
-- Link
pefToShowS (Function Link Source) = shows . getSource . link
pefToShowS (Function Link Destination) = shows . getDestination . link
pefToShowS (Function Link ID) = error "Link ID UNIMPLEMENTED"
pefToShowS (Function Link Offset) = error "Link Offset UNIMPLEMENTED"
pefToShowS (Function Link Payload) = shows . getPayloadType . link
-- Network
pefToShowS (Function Net Source) = shows . getSource . network
pefToShowS (Function Net Destination) = shows . getDestination . network
pefToShowS (Function Net ID) = shows . (IPv4.ipid).uIPv4.runHeader.network
pefToShowS (Function Net Offset) = shows . (IPv4.fragoff).uIPv4.runHeader.network
pefToShowS (Function Net Payload) = shows . getPayloadType . network
-- Transport
pefToShowS (Function Trans Source) = shows . getSource . transport
pefToShowS (Function Trans Destination) = shows . getSource . transport
pefToShowS (Function Trans ID) = error "Trans ID UNIMPLEMENTED"
pefToShowS (Function Trans Offset) = error "Trans Offset UNIMPLEMENTED"
pefToShowS (Function Trans Payload) = error "Trans Payload"



pefToCompare :: Bool -> PacketEquivalencyFunction -> (Message -> Message -> Bool)
pefToCompare _ AppFunction = equalMessageBy $  map w2c .  BL.unpack . application
pefToCompare f FileMatchAware = (filematch f)
-- Link
pefToCompare _ (Function Link Source) = equalMessageBy $  getSource . link
pefToCompare _ (Function Link Destination) = equalMessageBy $  getDestination . link
pefToCompare _ (Function Link ID) = error "Link ID UNIMPLEMENTED"
pefToCompare _ (Function Link Offset) = error "Link Offset UNIMPLEMENTED"
pefToCompare _ (Function Link Payload) = equalMessageBy $  getPayloadType . link
-- Network
pefToCompare _ (Function Net Source) = equalMessageBy $  getSource . network
pefToCompare _ (Function Net Destination) = equalMessageBy $  getDestination . network
pefToCompare _ (Function Net ID) = equalMessageBy $  (IPv4.ipid).uIPv4.runHeader.network
pefToCompare _ (Function Net Offset) = equalMessageBy $  (IPv4.fragoff).uIPv4.runHeader.network
pefToCompare _ (Function Net Payload) = equalMessageBy $  getPayloadType . network
-- Transport
pefToCompare _ (Function Trans Source) = equalMessageBy $  getSource . transport
pefToCompare _ (Function Trans Destination) = equalMessageBy $  getSource . transport
pefToCompare _ (Function Trans ID) = error "Trans ID UNIMPLEMENTED"
pefToCompare _ (Function Trans Offset) = error "Trans Offset UNIMPLEMENTED"
pefToCompare _ (Function Trans Payload) = error "Trans Payload"



hashMessage :: [PacketEquivalencyFunction] ->  Message -> Int32
hashMessage lst m = hashString . loop lst $ ""
  where
    loop (x:[]) = pefToShowS x m 
    loop (x:xs) = (pefToShowS x m).(loop xs)



equalMessageBy :: (Eq b)=>(Message->b) ->Message->Message->Bool
equalMessageBy f a b = (f a) == (f b)

apply :: a -> (a->b) -> b
apply a f = f a  


filematch justEQ a =  ((||) justEQ) . null . intersect (allfiles a) .  allfiles
allfiles = map sndT . matches


doit justEQ a b = and . map (apply b) . map (apply a) . map (pefToCompare justEQ) . pkteqfun $ a 
mergeMessage :: Bool -> Message -> Message -> Bool
mergeMessage = doit
--  and . map (apply b) . map (apply a) $ [filematch,nettype,nonipv4,netipid,netfo,netprot,nettype,transsrc,transdst,netsaddr,netdaddr]

type FlowID = (Address,Address,Address,Address)

data ParseState = ParseState
  { cnt :: Integer
  , flows :: M.Map FlowID Integer }


initParseState :: ParseState
initParseState = ParseState 0 M.empty

nextParseState :: Message->ParseState->(ParseState,Integer)
nextParseState m p = next (m2fid m) p
  where
    m2fid m = if (netS m)>(netD m) then (netS m,netD m,tranS m, tranD m) else (netD m,netS m,tranD m, tranS m)

    netS = getSource.network  

    netD = getDestination.network  

    tranS = getSource.transport

    tranD = getDestination.transport 

    next :: FlowID -> ParseState -> (ParseState,Integer)
    next fid a@(ParseState cnt fls) = 
      case M.lookup fid fls of
        Just x -> (a,x)         
        Nothing -> (ParseState (cnt+1) $ M.insert fid cnt fls,cnt)

getMessage :: [PacketEquivalencyFunction]->PacketData->Message
getMessage pef pd =
           let (l,lp) = getLinkFrame (PayloadTypePhysical Ethernet) (datacontents pd)
               (n,np) = getNetworkPacket (getPayloadType l) (lp)
               (t,tp) = getTransportDatagram (getPayloadType n) (np)
               m = Message pef 0 0 (len pd) (PayloadTypePhysical Ethernet) l n t (BL.fromChunks [tp]) [(ts pd,pcapfile pd,pktoffset pd)] 
               i = uniqueID pef m 
               u = (getFileString.pcapfile $ pd) ++ (show.pktoffset $ pd) in
            case i of
              Just x -> m { huuid = x  }
              Nothing -> m { huuid = hashString u }
              


data SFR f = forall a. Show a => SFR (f -> a)
showSFR (SFR f) a = shows $  f  a

extraInformation :: Message -> [ShowS]
extraInformation a = extraInformation' 
               (unwrapPhysical $ physical a) 
               (unwrapLink $ getPayloadType $ link a, runHeader $ link a) 
               (unwrapNetwork $ getPayloadType $ network a, runHeader $ network a) 
               (runHeader $ transport a)
               (application a)
  where
    unwrapPhysical a = case a of PayloadTypePhysical b -> b
    unwrapLink a = case a of PayloadTypeLink b -> b
    unwrapNetwork a = case a of PayloadTypeNetwork b -> b
   
    extraInformation':: 
      PhysicalPayloadType                       ->
      (LinkPayloadType,LinkFrame)               ->
      (NetworkPayloadType,NetworkPacket)        -> 
      TransportDatagram -> ApplicationData      -> 
      [ShowS]

    extraInformation' Ethernet (IPv4,LinkEthernetFrame l) (TCP,NetworkIPv4Packet n) (TCPTransportDatagram t) a =
      (++)
      (map (flip ($) t . showSFR)
      [ SFR TCP.sequenceN 
      , SFR TCP.acknowledgementN
      , SFR TCP.offset
      , SFR TCP.checksum
      ]) 
      []
      -- $ shahash a

    extraInformation' Ethernet (IPv4,LinkEthernetFrame l) (UDP,NetworkIPv4Packet n) (UDPTransportDatagram t) a =
      (++) 
      (map (flip ($) t . showSFR) [ SFR UDP.checksum  ]) 
      []
      -- $ shahash a

    extraInformation' _ _ _ _ _ = []


    shahash a = 
      case B.null a of 
        True -> [showString "-1"]
        False -> [shows . sha256 . BL.pack . B.unpack $ (a::B.ByteString)]
 
    




instance ManageablePacketEvent Message ParseState where
    getTimestamp a =  fstT.head.matches $ a


    -- | Ideally I would like merge to be:
    -- @
    --   merge = mergeMessage False
    -- @
    -- The problem with this is that while it will significant
    -- cut down on compares because packets from the same packet
    -- dump should never be merged ( maybe this is strange 
    -- constraint to enforce anyway ) it will make it very
    -- cumbersome for the current implemented 'PacketEventManager' 
    -- to remove packets on merges.  This indicates that this
    -- Type Class still needs some work in terms of abstracting
    -- what is really necessary for a 'PacketEventManager' to 
    -- know.
    merge = mergeMessage False
    equal = mergeMessage True


    mergePacketEvent a b = 
        a { matches = nmlst }
        where
          nmlst = foldl (\a e -> insertBy compareMI e a) (matches a) (matches b)
          compareMI a b = compare (fstT a) (fstT b) 
          chk = (length.matches)

    getPacketEvent pef pd Nothing = 
        let m = getMessage pef pd in 
        let (s,c) = nextParseState m initParseState in
        (m {fid = c},s)

    getPacketEvent pef pd (Just state) = 
        let m = getMessage pef pd in 
        let (s,c) = nextParseState m state in
        (m {fid = c},s)

    -- | TODO - output show be expressed in terms of SFR's
    -- | TODO - This output is not as robust as I would like it to be. More specifically if the a file name contains whitespace or commas
    -- parsing will become much harder.
    processPacketEvent a  = 
        let output = (shows.fid $ a).csep.
                     (shows.wirelength $ a).csep.
                     (shows.physical $ a).csep.
                     (shows.getSource.link $ a).csep.
                     (shows.getDestination.link $ a).csep.
                     (shows.getPayloadType.link $ a).csep.
                     (shows.getSource.network $ a).csep.
                     (shows.getDestination.network $ a).csep.
                     (shows.getPayloadType.network $ a).csep.
                     (shows.getSource.transport $ a).csep.
                     (shows.getDestination.transport $ a).csep.
                     (case (foldl (\ acc (t,pf,po) -> Just $ (case acc of Nothing -> id; Just x -> x.rsep;).bsep.(shows t).esep.(showString (f pf)).esep.(shows po).eesep) Nothing (matches a)) of
                       Nothing -> id 
                       Just x -> x). csep 
                     --(case extraInformation a of [] -> id; xs -> foldl (\ acc s -> acc.s.csep) id xs )
                       in
                   
        putStrLn $ output ""
      where
        csep = (showString " ")
        rsep = (showString "|")
        bsep = (showString "(")
        esep = (showString ",")
        eesep = (showString ")")
        f    = (getFileString)

    initialPacketEvent _ = return ()

    hashPacketEvent a = huuid a

    mergeLength a = length.matches $ a
