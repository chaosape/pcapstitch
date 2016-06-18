--------------------------------------------------------------------------------
-- | 
--  Copyright: Dan DaCosta 2010, Chaosape LLC
--  License: GPLv3
--  Maintainer: chaosape@chaosape.com
--  Stability: expermental
--  Portability: non-portable
--
-- Description: This module is responsible for handling data that implements the
-- ManageablePacketEvent type class and applying the implemented interface.  
-- The PacketEventManager is built to maintain in memory packets by time, the 
-- amount of time by which data will be considered fresh ( and therefore not
-- processed using processPacketEvent ) is determined through command line 
-- arguments.  
--------------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
--TODO The mergesuccess function in runPacketEventManager' should somehow handle 
-- the merge count command line argument. It currently does not and simply assumes
-- the merge count is always two.  So any command line arguments provided will be
-- ignored.
--TODO runPacketEventManager should probably be handled via the State monad.
--TODO runPacketEventManager documentation needs to be cleaned up.
module Network.PcapStitch.PacketEventManagement 
  ( runPacketEventManager )
where
import Control.Monad
import Data.Int
import Data.Sequence
import qualified Data.Sequence as DS
import Data.HashTable as HT
import Data.Maybe
import qualified Data.Foldable as F
import Network.PcapStitch.ManageablePacketEvent 
import Network.PcapStitch.PcapSources
import Network.PcapStitch.Options (Options(..))
import Network.PcapStitch.Timestamp
import Network.PcapStitch.Message
import Prelude hiding (length,splitAt)

-- | This function will process all of the packets from pcap sources according to the ManageablePacketEvent 
-- type class implementation. The process behaves something like this
-- 1) Get the next packet in time from pcap sources.
-- 2) Parse the packet according to the getPacketEvent implementation.
-- 3) Test to see if this packet event has another packet event to merge with.
-- 4) If it does, merge it and if the merge count is met process the packet event using processPacketEvent.
-- 5) If it doesn't, queue it as long as its time minus the most recent packet in the queue is less than 
-- the unstitched time provided via the command line. If this fails process the packet event using
-- processPacketEvent.
-- 6) When all packets have be read process the remaining packets in the queue .
-- Half implemented chan use 

-- MUTABLE HASHTABLE Version
data MergeOrEqual a = Merge a | Equal a

mergeorequal :: (ManageablePacketEvent storage storagestate) => MergeOrEqual storage -> MergeOrEqual storage -> Bool
mergeorequal (Merge a) (Merge b) = merge a b 
mergeorequal (Equal a) (Merge b) = equal a b  
mergeorequal (Merge a) (Equal b) = equal a b  
mergeorequal (Equal a) (Equal b) = equal a b  

hashMergeOrEqual (Merge a) = hashPacketEvent a 
hashMergeOrEqual (Equal a) = hashPacketEvent a 



-- TODO: Where ever possible use viewl and viewr instead of indexes
-- | This data structure maintains state between recurses of the runPacketEventManager function.
data (ManageablePacketEvent a s) => PacketEventManagementMutableHash a s = PacketEventManagementMutableHash
 {
  -- | The options provided on the command line
  opts :: Options,
  -- | A list of parsed packet data structures ordered by time
  chronoOrdered :: Seq a ,
  completeChronoOrdered :: Seq a,
  ht :: HT.HashTable (MergeOrEqual a) a,
  pktstate :: Maybe s,
  longestChronoLength :: Int,
  currentTime :: Timestamp
 }


(!-!) = index


--binsearch :: (ManageablePacketEvent a s) => a -> [a] -> Maybe Int
binsearch a lst = loop 0 $ (length lst)-1
  where
    loop low hi
    -- {-# SCC "hi_low_comparison" #-} 
      | hi < low = Nothing
      -- {-# SCC "mid_comparison" #-}
      | otherwise = 
         case compare (ts test) atime of
          -- {-# SCC "loop_low" #-}
          GT ->  loop low $ mid-1
          -- {-# SCC "loop_high" #-}
          LT ->  loop (mid+1) hi
          -- {-# SCC "loop_equal" #-}
          EQ ->  if ahash == (hs test)
                  then Just mid
                  -- {-# SCC "walk" #-} 
                  else Just . head . catMaybes $ [walkf (mid+1), walkb (mid-1)]
      where
        mid = (+) low $ div (hi-low) 2 
        test = lst !-! mid
        
    ts = getTimestamp 
    hs = hashPacketEvent 
    ahash = hashPacketEvent a
    atime = getTimestamp a

    walkg f m = loop m 
      where
        loop m = 
          if atime == (ts . (!-!) lst $ m)
            then if ahash == (hs . (!-!) lst $ m) 
                   then Just m
                   else loop . f $ m
            else Nothing

    walkf m = walkg ((+) 1) $ m
    walkb m = walkg (flip (-) 1) $ m



perPacketData ::  (ManageablePacketEvent a s)
              => Options 
              -> PacketData 
              -> (PacketEventManagementMutableHash a s)
              -> IO (PacketEventManagementMutableHash a s)
perPacketData options pkt mstate = (perpkt options mstate pktevent) >>= return'
  where
    pkteventandpktstate = getPacketEvent (packetEquivalencyFunction options) pkt . pktstate $ mstate
    pktevent = fst pkteventandpktstate
    pstate = snd pkteventandpktstate
    return' a =  return $ a {pktstate = Just pstate}
  
perpkt :: (ManageablePacketEvent a s) 
       => Options 
       -> PacketEventManagementMutableHash a s 
       -> a 
       -> IO(PacketEventManagementMutableHash a s)
perpkt options state' pktevent = purge' >>=  lookup >>= act >>= lastts 

  where
    purge'                = purge options state'
    lookup a              = liftM ((,) a) $ HT.lookup (ht a) (Merge pktevent)
    act (a,Nothing)       = mergefail  options a pktevent
    act (a,(Just oldpkt)) = mergesuccess options a pktevent oldpkt
    lastts s              = 
      if (currentTime s) < (getTimestamp pktevent)
        then return $ s { currentTime = getTimestamp pktevent }
        else return $ s     
    




-- What is done when we have no other packet events to merge with
mergefail options state pkt = insert >> lengthchk >>= return'
 where 
{-   hrzninfo     = 
     "New Horizon List Length = "
     ++(show (length.chronoOrdered $ state))
     ++". Current Horizon Time = "
     ++(show diff)-}
   insert       = HT.insert (ht state) (Merge pkt) pkt
   lengthchk    = return state
       {-if (&&) (not . null . chronoOrdered $ state )
          . (<) 100 
          . (-) (length . chronoOrdered $ state )
          . longestChronoLength $ state
         then infoOutput options hrzninfo 
              $ return 
              $ state { longestChronoLength = length.chronoOrdered $ state }
         else  return $ state -}

--   return' a   = return $ a { chronoOrdered = (chronoOrdered a)++[pkt]} 
   return' a   = return $ a { chronoOrdered = possibleOutOfOrderInsert options state fnchrono pkt} 

   fnchrono = chronoOrdered state


--   diff        = (-) ( getTimestamp . last . chronoOrdered $ state) 
--                 $ ( getTimestamp . head . chronoOrdered $ state)

dropAt :: Int -> Seq a -> Seq a
dropAt i lst = f >< b
  where
    split = splitAt (i+1) lst
    f' = fst split
    b = snd split
    f = handle (viewr f')
    handle ((:>) xs x) = xs





insertByS :: ( a -> a -> Ordering ) -> a -> Seq a -> Seq a
insertByS f e lst = loop empty (viewl lst)
  where
   loop _   EmptyL          = lst |> e
   loop acc ((:<) x xs)     = 
     case f e x of
       GT -> loop (acc|>x) . viewl $ xs
       _  -> (acc|>e|>x)><xs

possibleOutOfOrderInsert options state lst pkt
  | (getTimestamp pkt) >= (currentTime state) = lst |> pkt
  | otherwise = 
    {-warningOutput options "Out of Order Packet Detected -> Re-ordering" 
    $-} insertByS compareMS pkt lst
  where 
    compareMS a b = compare (getTimestamp a) (getTimestamp b)
  




-- What is done whe we find another packet to merge with
mergesuccess options state newpkt oldpkt = insert'
  where
    -- Take out the already stored packet from the chronologically ordered list
    --  {-# SCC "drope" #-}
    drope   = binsearch oldpkt $ chronoOrdered state
    nchrono Nothing  = 
      error 
        $ "Binsearch failure for packet"
        ++(show oldpkt)++"---"
        {- ++((foldl (\ a b -> a . (shows b) . (showString "\n")) id  (chronoOrdered state)) "")-}
    -- {-# SCC "ncrhono" #-}
    nchrono (Just i) =   dropAt i $ chronoOrdered state --uncurry (\ a -> (++) $ init a) $ splitlst i

    -- {-# SCC "fnchrono" #-}
    fnchrono =  nchrono drope
    -- {-# SCC "cchrono" #-}
    cchrono  =  completeChronoOrdered state

    -- {-# SCC "splitlst" #-}
    splitlst      i  = splitAt (i+1) $ chronoOrdered state 
    -- Merge the old packet with the new packet
    -- {-# SCC "mpkt" #-}
    mpkt    =  (mergePacketEvent oldpkt newpkt) 
    -- Take out the alread stored packet from the merge check table
    --delete  = HT.delete (ht state) (Equal oldpkt)
    -- {-# SCC "mlcheck" #-}
    mlcheck =  mergeLength mpkt >= stitchesPerPacket options
    -- Insert the merged packet
    insert  =  HT.update (ht state) (Equal mpkt) $ mpkt
    -- {-# SCC "insert'" #-}
    insert'  =  
      if mlcheck 
        then HT.delete (ht state) (Equal mpkt)>>complete
        else (HT.update (ht state) (Equal mpkt) $ mpkt)>>incomplete

    {-merror  = 
      if mlcheck 
        then error "runPacketEventManager: Too Many Merges!!!"
        else return ()          -}
    --Merge and remove the packet from the hashtable and the chrono queue
    --return' = return $ state { chronoOrdered = (nchrono drope)++[mpkt] }
    -- {-# SCC "incomplete" #-}
    incomplete =  return $ state {chronoOrdered = possibleOutOfOrderInsert options state fnchrono mpkt}
    -- {-# SCC "complete" #-}
    complete =  return $ state { chronoOrdered = fnchrono,completeChronoOrdered = possibleOutOfOrderInsert options state cchrono mpkt}


--    handleoutofordercase mpkt [] = mpkt:[]
--    handleoutofordercase mpkt xs 
--      | (getTimestamp mpkt) >= (getTimestamp . last $ xs) = 

{-
mergeOrdered xs ys = loop xs ys
  where
    loop [] ys = ys
    loop xs [] = xs 
    loop a@(x:xs) b@(y:ys) 
      | getTimestamp x > getTimestamp y = y:loop a ys
      | getTimestamp x < getTimestamp y = x:loop xs b
      | otherwise = x:loop xs b
-}
mergeOrdered' g xs ys = loop 0 0
  where
    lenx = length xs
    leny = length ys
    ts l = getTimestamp . index l 
    loop i j  
      | i>=lenx && j>=leny = return ()
      | i>=lenx = (g.index ys $j)>>(loop i (j+1)) 
      | j>=leny = (g.index xs $i)>>(loop (i+1) j)
      | otherwise =
        if (ts xs i)> (ts ys j) 
          then (g.index ys $j)>>(loop i (j+1))
          else (g.index xs $i)>>(loop (i+1) j)




span' g lst = loop 0
  where 
    len = length lst
    loop i 
      | i >= len = (lst,empty)
      | otherwise =
        if g (index lst i)
          then loop (i+1)
          else splitAt i lst

purge options state = process>>delete>>return'
  where 
    cremovessandckeeps = span' (purgeCompare options state) $ completeChronoOrdered state 
    iremovesandikeeps = span' (purgeCompare options state) $ chronoOrdered state
    iremoves = fst iremovesandikeeps
    ikeeps = snd iremovesandikeeps
    cremoves = fst cremovessandckeeps
    ckeeps = snd cremovessandckeeps
    process = mergeOrdered' processPacketEvent iremoves cremoves
    delete = F.mapM_ (\a -> HT.delete (ht state) (Equal a)) iremoves
    return' = return $ state { chronoOrdered = ikeeps, completeChronoOrdered = ckeeps}


purgeCompare options state a = (difference > (packetHorizon options))
  where 
    difference =  
--      (-) (getTimestamp . last . chronoOrdered $ state) (getTimestamp a)
      (-) (currentTime state) (getTimestamp a)

{-
seqToList = loop . viewl
  where
    loop EmptyL = []
    loop ((:<) x xs) = (:) x . loop . viewl $ xs
-}

runPacketEventManager pcs options = newmap >>= newstart >>= mapdata >>= finish
 where
   newmap     = HT.new mergeorequal hashMergeOrEqual
   newstart j = 
     return 
     $ ((PacketEventManagementMutableHash options (empty) (empty) j Nothing 0 (Timestamp 0 0)) 
         :: PacketEventManagementMutableHash Message ParseState)
   mapdata    = foldPacketDataWith (perPacketData options) pcs
   finishc  s = completeChronoOrdered $ s
   finishi  s = chronoOrdered $ s
   finish   s = mergeOrdered' processPacketEvent (finishc s) (finishi s)

