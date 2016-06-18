--------------------------------------------------------------------------------
-- | 
--  Copyright: Dan DaCosta 2010, Chaosape LLC
--  License: GPLv3
--  Maintainer: chaosape@chaosape.com
--  Stability: expermental
--  Portability: non-portable
--
-- Description: This file presents an interface for packet capture files and 
-- functions for manipulating them.  
--------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | This module presents an interface to manipulate packet capture files.  
-- This is primarily done through functions and the  'PcapSources' object.
-- The other objects and functions are presented for 'PacketEventManagers'
-- to retreive information about packets. 
module Network.PcapStitch.PcapSources 
  ( PcapSource
  , ondeck
  , pcaphandle 
  , PacketData (..)
  , PcapFile 
  , getFileString
  , PcapSources
  , mkPcapSources 
  , foldPacketDataWith )
where
import qualified Data.ByteString as B
import System.Path
import Control.Monad
import Data.Maybe
import Network.Pcap
import Network.PcapStitch.Timestamp
import Data.List
--import Data.List.Stream 
--import Prelude hiding ((++),(!!),foldl,length,splitAt,span,head,map,tail)



-- | PcapSource stores information for one packet capture 
-- file.
data PcapSource = PcapSource 
  {
    _pcapfile   :: PcapFile
    -- ^ Stores the filename and path
  , _pcaphandle :: PcapHandle
    -- ^ This is the open reference to the packet capture
    -- file. It is used in many of the functions within
    -- the 'Network.Pcap' library.
  , _pktoffset  :: Integer
    -- ^ The current packet offset into the packet capture file.
  , _ondeck     :: (Timestamp,PacketDataSimple) 
    -- ^ The next packet that will be retrieved from the packet
    -- capture file. The first element is the 'Timestamp' associated
    -- with the on deck packet, the second element is the contents
    -- of the next on deck packet.
  }

ondeck :: PcapSource -- ^ The PcapSource where the on deck packet will
                     -- be retreived from
       -> (Timestamp,PacketDataSimple) 
       -- ^ The ondeck packet, the first element is the 'Timestamp' assciated
       -- with that packet and the second element is the packet content.
ondeck (PcapSource _ _ _ x) = x


pcaphandle :: PcapSource -- ^ The PcapSource where the the 'PcapHandle' will
                         -- be retreived from.
           -> PcapHandle -- ^ The file handled used for interacting with the 
                         -- file through the 'Network.Pcap' library.
pcaphandle (PcapSource _ x _ _) = x 



-- | The type that will store the filename and the path to packet capture file.
type PcapFile = Either AbsFile RelFile


-- | Gets the path and filename in string from a 'PcapFile'
getFileString :: PcapFile -- ^ The packet capture file that the
                          -- path and filename will be retrieved from.
              -> String   -- ^ The string representation of the 
                          -- packet capture files path and filename.
-- | It is an absolute path
getFileString (Right x) = getPathString x
-- | It is a relative path
getFileString (Left x) = getPathString x

-- | A representation of the raw data that is retrieved from 'Network.Pcap' 
-- for a packet.
data PacketDataSimple = PacketDataSimple 
     PktHdr        -- ^ The libpcap header stored with all captured packets
     B.ByteString  -- ^ The bytestring representation of the packet from
                   -- the link layer up.

-- | This is a synthesis into a more explicit form of what is retreived from 
-- the Pcap library.  Specifically the 'PktHdr' has be replaced by the actual
-- values that PcapStitch needs.
data PacketData = PacketData
 {pcapfile :: PcapFile -- ^ Stores the filename and path that this packet 
                       -- came from.
  , pktoffset :: Integer -- ^ The packet offset into the packet capture
                         -- file for this packet.
  , ts :: Timestamp      -- ^ The time this packet was recorded in the
                         -- packet capture file.
  , len :: Integer       -- ^ The wire length of the packet (layer 2+)
  , datacontents :: B.ByteString -- ^ The contents of the packet
 } deriving(Show)

instance Show PcapSource where
  showsPrec _ (PcapSource f _ o _) = 
    (showString "(")
    . (showString $ getFileString f)
    . filler
    . (shows o)
    . filler
    . (showString ")")
    where
      filler = (showString ",")


-- | Used to help enforce ordering.  This list /MUST ALWAYS/ be orderd from earlist packet on deck to latest packet on deck
data OrderedPcapSources = OrderedPcapSources [PcapSource] deriving (Show)

-- | Get the next PcapSource with the earliest packet on deck.  This function 
-- will remove the packet source from the list. So if it should be used again 
-- it should be added back with addPcapSource.
getEarliestPcapSource :: OrderedPcapSources -- ^ A list of PcapSources 
                                            -- ascendingly ordered by on deck 
                                            -- packet timestamp
                      -> (PcapSource,OrderedPcapSources) -- ^ A tuple where the 
                      -- first element is the next PcapSource that should be 
                      -- read from and the second element is the list of 
                      -- ordered PcapSources with the first element removed.
getEarliestPcapSource (OrderedPcapSources pss) = 
  (head pss,OrderedPcapSources $ tail pss) 

-- | Insert a 'PcapSource' back into a 'OrderedPcapSources' according to the
-- candidate 'PcapSource''s on deck packet 'Timestamp'
addPcapSource :: PcapSource  -- ^ The candidate for insertation 'PcapSource'
              -> OrderedPcapSources  -- ^ The 'OrderedPcapSources' that the
              -- 'PcapSource' will be inserted into
              -> OrderedPcapSources -- ^ The new 'OrderedPcapSource' with
              -- the candidate 'PcapSource' inserted
addPcapSource ps (OrderedPcapSources pss) = 
  OrderedPcapSources $ insertBy pcapsourcecmp ps pss
  where
    timestamp = fst.ondeck
    pcapsourcecmp ps1 ps2 = compare (timestamp ps1) (timestamp ps2)


-- | A collection of 'PcapSource's. Behavior is unspecified if any
-- of the sources are empty.
data PcapSources = PcapSources 
  Timestamp -- ^ The 'Timestamp' of the packet to last be removed
  -- from any of the sources.
  OrderedPcapSources -- ^ An ordered list of 'PcapSource's 
  deriving (Show)

-- | Convert a list of 'PcapFile's that store filename and paths
-- of candidate packet capture files for reading and open them all
-- NOTE -- It is possible that this function will fail for large 
-- sizes of the input list because maximum open file descriptor
-- counts can be exceeded.
mkPcapSources :: [PcapFile] -> IO PcapSources
mkPcapSources pfs = do
  -- Make 'PcapSource's out of all the files
  pss <- mapM perfile pfs
  -- Return a 'PcapSources'
  return 
    $PcapSources 
      -- Start the 'PcapSources' 'Timestamp' at 0
      (mkTimestamp 0 0) 
      -- Sort the list of valid 'PcapSource's
      (OrderedPcapSources $ sortBy pcapsourcecmp (catMaybes pss))
  where
    timestamp = fst.ondeck

    pcapsourcecmp ps1 ps2 = compare (timestamp ps1) (timestamp ps2)
    
    
    getts pkthdr = 
      mkTimestamp 
        (fromIntegral.hdrSeconds$pkthdr) 
        (fromIntegral.hdrUseconds$pkthdr)

    perfile :: PcapFile -- ^ The packet capture file that will
            -- be opened and convert to a 'PcapSource'
            -> IO (Maybe PcapSource) -- ^ An attemp to open
            -- the file and construct a 'PcapSource'. If the 
            -- packet capture file is empty 'Nothing' will
            -- be returned.
    perfile pf = do
      ph <- openOffline $ getFileString pf
      (pkthdr,bs) <- nextBS ph
      return
        $if B.null bs 
           then Nothing 
           else 
             Just 
               $PcapSource pf ph 1 (getts pkthdr,(PacketDataSimple pkthdr bs))



--foo :: StateT PcapSources IO (Maybe PacketData) -> StateT PcapSources IO ()
{-foo fb = do
   t <- fb
   let (a,b) = t
   return ((),b) 
-}
{-

mapPacketDataWith ::
                  ( PacketData -> state -> IO state ) 
                  -> state 
                  -> StateT PcapSources IO state
mapPacketDataWith f s = mapStateT (convert s) nextPacketData
  where 
    convert s m1 = do
      stateT <- liftIO m1
      let (pkt,pcs) = stateT
      case pkt of
        Nothing -> return (s,pcs) 
        Just pkt' -> do 
                     s' <- liftIO . f pkt' $ s
                     s' `seq` convert s' m1
mapPacketDataWith f s = do
  pkt <- nextPacketData
  case pkt of
    Nothing -> return s
    Just pkt' -> do 
                 s' <- liftIO . f pkt' $ s
                 mapPacketDataWith f s'
-}


foldPacketDataWith ::
                  ( PacketData -> state -> IO state ) 
                  -> PcapSources
                  -> state 
                  -> IO state
foldPacketDataWith _ (PcapSources _ (OrderedPcapSources [])) s = return s
foldPacketDataWith f pcs s = loop pcs s 

  where

    loop (PcapSources _ (OrderedPcapSources [])) s = return s
    loop pcs s = (continue pcs) >>= process s >>= (uncurry loop)
 
    process s (psc',pkt) = liftM ((,) psc') $  f pkt $ s  


    getts pkthdr = 
      mkTimestamp 
        (fromIntegral.hdrSeconds$pkthdr) 
        (fromIntegral.hdrUseconds$pkthdr)
        
    modpcapsource ps pkthdr bs cnt= 
      ps { _pktoffset = cnt + 1
          , _ondeck = (getts pkthdr,(PacketDataSimple pkthdr bs)) }

    readdpcapsource ps ops pkthdr bs cnt = 
      addPcapSource (modpcapsource ps pkthdr bs cnt) ops

    continue (PcapSources _ opcs) = do  
        let (ps,ops) = getEarliestPcapSource opcs
        let cnt = _pktoffset ps
        let tuple = ondeck ps
        let (ts',PacketDataSimple pkthdr' bs') = tuple
        (pkthdr,bs) <- (nextBS.pcaphandle$ps)
        let newpsc = 
              PcapSources ts' 
                $if (B.null bs) 
                  then ops 
                  else readdpcapsource ps ops pkthdr bs cnt
--        if B.null bs
--          then do
--               close
        return
          $(,) newpsc 
            $PacketData 
              (_pcapfile ps) 
              (_pktoffset ps) 
              ts' 
              (fromIntegral.hdrWireLength$pkthdr') bs'

--             let s = f (fromJust pkt) s
--             return (s,state)--(convert s') m1



{-
      pkt <- manex
  case pkt of
    Nothing -> return s
    Just pkt' -> do
                 s' <- f pkt' s
                 mapPacketDataWith f s' 
 
mapPacketDataWith f = repeat $ mapStateT f' nextPacketData 
 where
  f' x = do 
    t <- liftIO . g $ x
    let (e,s) = t
    if isJust e 
      then return (Just . f $ e,s)
      else return (Nothing,s)

  g x = do
    t <- x
    let (e,s) = t
    return (e,s)
-}
--    e' <- f e
--    f e
--    return (,s)





{-
-- | This function makes the retrieval of the next 
-- earliest packet a state transition machine. If 
-- 'Nothing' is returned there is no more data to read.
nextPacketData :: StateT PcapSources IO (Maybe PacketData)
nextPacketData = do
  pcs <- get
  case pcs of 
    (PcapSources _ (OrderedPcapSources [])) -> return Nothing
    _ -> continue pcs
  where
    getts pkthdr = 
      mkTimestamp 
        (fromIntegral.hdrSeconds$pkthdr) 
        (fromIntegral.hdrUseconds$pkthdr)
        
    modpcapsource ps pkthdr bs cnt= 
      ps { _pktoffset = cnt + 1
          , _ondeck = (getts pkthdr,(PacketDataSimple pkthdr bs)) }

    readdpcapsource ps ops pkthdr bs cnt = 
      addPcapSource (modpcapsource ps pkthdr bs cnt) ops

    continue (PcapSources _ opcs) = do  
        let (ps,ops) = getEarliestPcapSource opcs
        (pkthdr,bs) <- liftIO.nextBS.pcaphandle$ps
        let cnt = _pktoffset ps
        let (ts',PacketDataSimple pkthdr' bs') = ondeck ps
        let newpsc = 
              PcapSources ts' 
                $if (B.null bs) 
                  then ops 
                  else readdpcapsource ps ops pkthdr bs cnt
        put newpsc
        return 
          $Just 
            $PacketData 
              (_pcapfile ps) 
              (_pktoffset ps) 
              ts' 
              (fromIntegral.hdrWireLength$pkthdr') bs'


-}