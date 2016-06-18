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
module Network.PcapStitch.Classes 
  ( HasSource(..)
  , HasDestination(..)
  , HasPayloadType
  , PayloadType
  , getPayloadType )
where
import Network.PcapStitch.Address 
import Network.PcapStitch.Header.PayloadType



class HasSource a  where
      getSource :: a -> Address


class HasDestination a where
      getDestination :: a -> Address 



class HasId a where
      getId :: a -> Integer

class HasPayloadType a where 
    getPayloadType :: a -> PayloadType
