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
{-# LANGUAGE ExistentialQuantification #-}
module Network.PcapStitch.Address
(
 Address(..)
) where
import qualified Data.ByteString as B
import Network.PcapStitch.Header.PayloadType

data Address = forall a.(Show a, Ord a,Eq a,Integral a)=> Address 
  {
    t::PayloadType, 
    a::a -- This should probably be a sealed field but I dont want to write all the type classes right now
  }
  | NoAddress
  | BadAddress PayloadType 

instance Eq Address where
  (==) (Address pa aa) (Address pb ab) = (pa == pb) && ((fromIntegral aa) == (fromIntegral ab))
  (==) _ _ = False

instance Ord Address where
  compare NoAddress NoAddress = EQ
  compare NoAddress (Address _ _) = LT
  compare NoAddress (BadAddress _) = GT
  compare (BadAddress x) (BadAddress y) =compare x y
  compare (BadAddress _) NoAddress = LT
  compare (BadAddress _) (Address _ _) = LT
  compare (Address pa aa) (Address pb ab) =
    let fc = compare (pa) (pb) in
    if fc == EQ then compare (fromIntegral aa) (fromIntegral ab) else fc
    where
      f = fromIntegral
  compare (Address _ _) _ = GT

instance Show Address where
  showsPrec p (Address l a) = {-(showsPrec p l).(showString ",").-}(showsPrec p a) 
  showsPrec p NoAddress = (showString "NoAddress") 
  showsPrec p (BadAddress t)= {-(showsPrec p t).(showString ",").-}(showString "BadAddress") 

