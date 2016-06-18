--------------------------------------------------------------------------------
-- |
--  Copyright: Dan DaCosta 2010, Chaosape LLC 
--  License: GPLv3
--  Maintainer: chaosape@chaosape.com
--  Stability: expermental
--  Portability: non-portable
--
-- Description: This file describes the ManageablePacketEvent type class and
-- provides a couple example implementations of this type class.
--------------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Network.PcapStitch.ManageablePacketEvent where
import Data.Int
import Network.PcapStitch.PcapSources
import Network.PcapStitch.Timestamp
import Network.PcapStitch.PacketEquivalencyFunction



-- | This class defines a set of methods that can be used to parse and analyze data from 
-- a packet trace. The storage type variable is meant to provide a way to store some packet
-- based information within the PacketEventManager. The monad and monadictype type variables 
-- describe how storage should be removed from the PacketEventManager queues either because 
-- the storage has been merged successfully or because the storage is too old compared to new 
-- incoming storage.  The storagestate type variable allows some notion of state to be managed
-- over multiple packet parses.  For example this could be used to track TCP state.
-- mergeCompare/hashPacketEvent make the assumption that there is a way to make any packet 
-- sent unique.  Packets are certainly unique by the timestamp pcap records on monitoring 
-- the packet but the problem is this is not invariant to a single packet moving through
-- the network and therefore could not be used to match a packet in one pcap file to a 
-- packet in another pcap file.  In some cases it may be impossible to generate a truly
-- unique but multiple pcap file invariant ways to compare and merge packets.  In this
-- case merging will happen in chronological order while reading from pcap sources.   

class (Show storage) => ManageablePacketEvent storage storagestate 
   | storage -> storagestate  where
    -- | A method to get a timestamp associated with this storage.  This timestamp will be
    -- used to evaluate storage freshness in the queue of PacketEventManager.  The primary
    -- purpose of this process is to be able to bound the amount of memory being used to 
    -- merge ManageblePacketEvents.
    getTimestamp :: storage -> Timestamp
    -- | This function is used to test if two packets can be merged. 
    merge :: storage -> storage -> Bool
    -- | This function is used to test if two packets are identical.  This IS NOT the same as 
    -- unique. 
    equal :: storage -> storage -> Bool
    -- | If mergeCompare evaluates to storages to be equivelent than this method
    -- will be called.
    mergePacketEvent :: storage -> storage -> storage 
    -- | This method should described how raw packet data should be parsed. The first return value
    -- is the storage type rendered from the PacketData and storage state.  If it is nothing
    -- then the processPacketEvent interface will never be called for this PacketData.
    getPacketEvent ::  [PacketEquivalencyFunction] -> PacketData -> Maybe storagestate -> (storage,storagestate)
    -- | This method will be called when storage is removed from the PacketEventManager
    -- queue.
    processPacketEvent :: storage -> IO ()
    -- | The starting value for removal of storage elements from the PacketEventManager
    -- queue.
    initialPacketEvent :: storage -> IO ()
    -- | Convert this storage unit into a recoverable hash key.  This string is not
    -- garuanteed to be unique.
    hashPacketEvent :: storage -> Int32
    -- | Then number of merges this packet has been involved in
    mergeLength :: storage -> Int
{-    
import Control.Monad
import Data.HashTable
-- | A simple data structure that will be used as the storage type for the 
-- ManageablePacketEvent type class example implementation.
data SimpleExample = SimpleExample PacketData Int Int deriving (Show)
-- | Implementation of the ManageablePacketEvent type class to serve as an example
--instance ManageablePacketEvent SimpleExample Maybe () Int where
instance ManageablePacketEvent SimpleExample Int where
    getTimestamp (SimpleExample a _ _) = (ts a)
    merge (SimpleExample a _ _) (SimpleExample b _ _) =(stringify a) == (stringify b)
        where
          stringify a = (getFileString (pcapfile a)) ++ (show (pktoffset a))  
    equal = merge
    mergePacketEvent (SimpleExample a as ac)  (SimpleExample b bs bc) =
      SimpleExample a as (ac+bc)
    mergeLength (SimpleExample _ _ c) = c
    getPacketEvent pd Nothing = (SimpleExample pd 1 1,2)
    getPacketEvent pd (Just state) = (SimpleExample pd state 1,state+1)
    processPacketEvent (SimpleExample a b _) = print ((show a)++(show b))
    initialPacketEvent _ = return ()
    hashPacketEvent (SimpleExample a _ _) = hashString $ stringify a 
        where
          stringify a = (getFileString (pcapfile a)) ++ (show (pktoffset a))  

-}