--------------------------------------------------------------------------------
-- | 
--  Copyright: Dan DaCosta 2010, Chaosape LLC
--  License: GPLv3
--  Maintainer: chaosape@chaosape.com
--  Stability: expermental
--  Portability: non-portable
--
-- Description: PcapStitch identifies the same packet from packet captures that 
-- have been collected at multiple points in a network simultaneously.  This is 
-- done by using header fields and a packet horizon ( a time based on maximum 
-- one-way latency in the network ).  Identification is done after collection 
-- has been completed. 
--------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall -Werror #-}
-- NOTE: If you want to use SourceGraph use Main.hs as the root not the
-- cabal file.  For some reason when you use the cabal file functions
-- are not found.
module Main where
-- import System
-- import System.Console.GetOpt
-- import IO
import Control.Monad
import Network.PcapStitch.Options
import Network.PcapStitch.PcapSources
import Network.PcapStitch.PacketEventManagement 


-- | This function will begin the machinery for stitching pcap files. It 
-- should be called only if the options have been parsed properly.  It will
-- convert a list of 'String's that represent dump file into PcapFiles.
mainsuccess :: Options      -- ^ The options provided to PcapStitch.        
            -> [String]     -- ^ A list of strings that represent paths and
                            --   file names to packet capture files
            -> IO ()        -- ^ This function will be causing side-effects
                            -- but returns no action value
mainsuccess  opts nonoptions = 
   join 
   -- Begin Packet Processing 
   . liftM (flip runPacketEventManager opts) 
   -- convert a list of files to a list of pcap files
   . mkPcapSources 
   . handlenonoptions 
   $ nonoptions
    

  
-- | This function will handle failed PcapStitch executions caused by improper 
-- use of command line arguments or bad input files
mainfailure :: [String]  -- ^ A list of strings that contain errors from 
                         -- improper command line use
            -> IO ()     -- ^ This function will be creating side-effects
                         -- but returns no action value
mainfailure = mapM_ print


-- | Entry point for PcapStitch
main :: IO () -- ^ This function will be creating side-effections
              -- but returns no action value
main = do
  (opts, nonOptions) <- parseOptions

  mainsuccess opts nonOptions
  --  then mainfailure errors
  --    $foldl (>>=) (return startOptions) actions

  --  input >>= output . filt
  -- Get the command line arguments
  -- args <- getArgs
  -- Parse the command line arguments 
  --let (actions, nonOptions, errors) = getOpt RequireOrder options args
  -- If there are no errors go to mainsuccess otherwise go to mainfailure
  --if length errors > 0 
  --  then mainfailure errors
    -- Roll the directives from the command line arguments into one data 
    -- structure
  --  els join . liftM (flip mainsuccess nonOptions ) 
  --    $foldl (>>=) (return startOptions) actions

