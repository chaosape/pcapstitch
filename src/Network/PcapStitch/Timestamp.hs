--------------------------------------------------------------------------------
-- | 
--  Copyright: Dan DaCosta 2010, Chaosape LLC TBD
--  License: GPLv3
--  Maintainer: chaosape@chaosape.com
--  Stability: expermental
--  Portability: non-portable
--
-- Description: Describes a TimeStamp data structure that is used throughout
-- PcapStitch to encode time associate with packet events.
--------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.PcapStitch.Timestamp (
  Timestamp(seconds,subseconds,Timestamp),
  mkTimestamp,
  timestamp_property_subtraction,
  timestamp_property_addition,
  upperopenbound,
  tsToDouble,
  TSUnit
) 
where
import Text.Printf

-- | A generalized polymorphic type representing the unit type of a 'TimeStamp'
type TSUnitG a = (Integral a, Num a) => a

-- | A concrete version of a 'Timestamp' unit type where an 'Int' will be used
-- to represent seconds and sub-seconds. 
type TSUnit = TSUnitG Int

-- | The maximum subseconds
upperopenbound :: Int
upperopenbound = 1000000

-- | The order of magnitude of upperopenbound
upperopenboundom :: Int
upperopenboundom = 
 round 
 . logBase 10 
 . (fromIntegral :: Int -> Double ) 
 $ upperopenbound

-- | A data structure that will refer to time as since the epoch.
-- [@seconds@] The seconds since the epoch.
-- [@subseconds@] The sub-seconds since the epoch.
data Timestamp = Timestamp 
  { seconds :: {-# UNPACK #-} !TSUnit
  , subseconds :: {-# UNPACK #-} !TSUnit }


-- | This will convert a timestamp composed of a second part and a subsecond
-- part into a timestamp.  This functio will fail if either of the integers
-- provided are negative.
mkTimestamp :: TSUnit -> TSUnit -> Timestamp
mkTimestamp s ss 
  | s>=0 && ss>=0 && ss<upperopenbound = Timestamp s ss
  | otherwise = error 
     $ "mkTimestamp: Both components must be non-negative and subsecond " 
     ++ "component must be less than " 
     ++ (show upperopenbound)


-- | 'Show' type class implementation for 'Time'tamp'
instance Show Timestamp where
  showsPrec _ (Timestamp s ss) = showString $ printf strformat s ss
    where
      strformat = "%d.%0"++(show upperopenboundom)++"d"

-- | 'Eq' type class implementation for 'Timestamp'
instance Eq Timestamp where
    (==) (Timestamp xs xus) (Timestamp ys yus) = (xs==ys) && (xus==yus)

-- | 'Ord' type class implementation for 'Timestamp'
instance Ord Timestamp where
    compare x y = 
      case compare (seconds x) (seconds y) of
        EQ -> compare (subseconds x) (subseconds y)
        v -> v
-- | 'Num' type class implementation for 'Timestamp'
instance Num Timestamp where
    (+) (Timestamp xs xus) (Timestamp ys yus) = (Timestamp (xs+ys+o) r)
      where
        fi :: (Integral a)=> a -> Double
        fi = fromIntegral
        r = mod (xus + yus) upperopenbound
        o = truncate $ (fi (xus + yus)) / (fi upperopenbound)

    (-) (Timestamp xs xus) (Timestamp ys yus) = ts (xs'-ys-o) r
      where
        ts a b = Timestamp (abs a) (abs b)
        fi :: (Integral a)=> a -> Double
        fi = fromIntegral
        r = mod (xus - yus) upperopenbound
        o = truncate $ (fi (xus - yus)) / (fi upperopenbound)
        xs'  = if yus>xus then xs-1 else xs

    abs = id 

    fromInteger x = Timestamp (fromInteger x) 0

    -- | XXX - Not implemented
    (*) _ _ = error "Num Timestamp * not defined."

    -- | XXX - Not implemented
    signum _ = error "Num Timestamp Signum not defined"


-- | A function to check correct subtraction of 'Timestamp'
timestamp_property_subtraction :: Double -> Double -> Bool
timestamp_property_subtraction = timestamp_property_binary (-) (-)

-- | A function to check correct addition of 'Timestamp'
timestamp_property_addition :: Double -> Double -> Bool
timestamp_property_addition = timestamp_property_binary (+) (+)


-- | A function that can test that 'Double' binary operator
-- and a 'Timestamp' operator return equivalent values when
-- given the same input.
-- TODO - This was written when I had a 'less elegant' 
-- understanding of Haskell.  I should probably give 
-- 'Timestamp' and instance of 'Arbitrary' and re-implement
-- this function.
timestamp_property_binary :: 
                          (Double->Double->Double)
                          ->(Timestamp
                          ->Timestamp
                          ->Timestamp)
                          ->Double 
                          -> Double 
                          -> Bool
timestamp_property_binary fa fb a' b' = 
    let a = abs a'
        b = abs b'
        (sa,ssa) = decompose a
        (sb,ssb) = decompose b
        dtsa = makeDouble sa ssa
        dtsb = makeDouble sb ssb
        ttsa = Timestamp sa ssa
        ttsb = Timestamp sb ssb
        result = (abs $ dtsa `fa` dtsb) - (tsToDouble $ ttsa `fb` ttsb)< unit in
        result
    where

      unit = 1/(fromIntegral upperopenbound)

--      opToStr :: Double->Double->Timestamp->Timestamp->String
--      opToStr da db ta tb = 
--        "("++(show da)++"-"++(show db)++"=="++(show $ abs $ da `fa` db)++")"++
--        "("++(show ta)++"-"++(show tb)++"=="
--        ++(show $ tsToDouble $ ta `fb` tb)++")"


      decompose :: Double -> (Int,Int)
      decompose i = (fromIntegral a,makeMicroseconds b)
        where 
          (a,b) = (properFraction i ::  (Int,Double))

      makeMicroseconds :: Double -> TSUnit
      makeMicroseconds x = round $ x * (fromIntegral upperopenbound)

-- | Convert a 'Timestamp' to a 'Double'
tsToDouble :: Timestamp -> Double
tsToDouble (Timestamp a b) = makeDouble a b

-- | Make two components of a 'Timestamp' into a double
makeDouble :: TSUnit  -- ^ The seconds component of the double
           -> TSUnit  -- ^ The sub-seconds component of the double
           -> Double
makeDouble s ss = 
  (fromIntegral s) + ((fromIntegral ss)/(fromIntegral upperopenbound))
