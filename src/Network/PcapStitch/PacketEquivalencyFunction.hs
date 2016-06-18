--------------------------------------------------------------------------------
-- | 
--  Copyright: Dan DaCosta 2010, Chaosape LLC
--  License: GPLv3
--  Maintainer: chaosape@chaosape.com
--  Stability: expermental
--  Portability: non-portable
--
--  Description: TODO
--------------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
module Network.PcapStitch.PacketEquivalencyFunction 
  (Layer(..)
  ,Field(..)
  ,PacketEquivalencyFunction(..)
  ,readPacketEquivalencyFunction
  ,fieldDescriptions
  )
where

data Layer = Link | Net | Trans | App | FileMatch deriving (Read,Show,Eq)
data Field = 
  Source 
  |Destination
  |ID
  |Offset
  |Payload
  deriving (Read,Show,Eq)


data PacketEquivalencyFunction =
  AppFunction 
  |FileMatchAware
  |Function Layer  Field
  deriving (Show)



fieldDescriptions :: [String]
fieldDescriptions = 
  [ "Link.Source -- Link layer source address"
  , "Link.Destination -- Link layer destination address"
  , "Link.ID -- UNIMPLEMENTED"
  , "Link.Offset -- UNIMPLEMENTED"
  , "Link.Payload -- The protocol being carried by the link layer"
  , "Net.Source -- Network layer source address"
  , "Net.Destination -- Network layer destination address"
  , "Net.ID -- IP ID"
  , "Net.Offset -- Fragementation Offset"
  , "Net.Payload -- The protocol being carried by the network layer"
  , "Trans.Source -- Transport layer source address"
  , "Trans.Destination -- Transport layer destination address"
  , "Trans.ID -- UNIMPLEMENTED"
  , "Trans.Offset -- UNIMPLEMENTED"
  , "Trans.Payload -- UNIMPLEMENTED"
  , "FileMatch -- Packets will only be merged if they did not come from the same trace file"
  , "App -- Hash of the application layer payload"]

readPacketEquivalencyFunction [] = []
readPacketEquivalencyFunction s =
  if test
    then func:[]
    else func:(myRead rest)
  where
    myRead = readPacketEquivalencyFunction
    spanOnOps a = '.'/=a&&'+'/=a 
    fstSpan = span spanOnOps s
    layer = read . fst $ fstSpan
    checkIfApp = 
      if (layer == App || layer == FileMatch)
         && ((null . snd $ fstSpan) 
            ||((head . snd $ fstSpan) == '+'))
         then True
         else False  
        
    sndSpan = span ((/=) '+') . tail . snd $ fstSpan
    field = read . fst $ sndSpan
    test  = (null . snd $ fstSpan) ||  (null . snd $ sndSpan)
    rest  = 
      if checkIfApp
        then if null  . snd $ fstSpan
             then [] 
             else tail . snd $ fstSpan
        else if null . snd $ sndSpan
             then []
             else tail . snd $ sndSpan
    func  = 
      if checkIfApp
        then AppFunction
        else Function layer field
      
      

