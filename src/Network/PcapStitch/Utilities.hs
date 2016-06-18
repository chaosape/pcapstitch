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
{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields #-}

module Network.PcapStitch.Utilities 
  -- * Utility functions
  ( wordsToInt
  , wordsToWord16
  , wordsToWord32
  , word32ToInt
  , biggestIntegral 
  , infoOutput
  , warningOutput 
  , getbgword
  , getbgword8
  , getbgword16
  , getbgword32 
  , getbgword64
  , debug)
where

import qualified Data.ByteString as B
import Data.Word
import Bits
import Debug.Trace 
import Network.PcapStitch.Options
import Data.Binary.Strict.BitGet

wordsToInt :: Bits a => Int -> [Word8] -> a
wordsToInt i l = foldr1 (.|.) shifted
  where
    z5 = reverse [0..i-1]
    shifted = zipWith shiftByte z5 l
    shiftByte b c = (fromIntegral c) `shiftL`  (b * 8)  

wordsToWord16 :: [Word8] -> Word16
wordsToWord16 (a:b:[]) = ((fromIntegral a) `shiftL` 8) .|. (fromIntegral b)
wordsToWord16 x = wordsToInt 2 x

wordsToWord32 :: [Word8] -> Word32
wordsToWord32 x = wordsToInt 4 x

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral


biggestIntegral ::(Integral a,Bits a)=>  B.ByteString -> a
biggestIntegral bs = (loop $! len) 0
  where
    len = flip (-) 1 $ B.length bs 
    loop i o
      | i < 0  = 0
      | otherwise = (+) (flip shiftL o $! (fromIntegral . B.index bs $! i)) $! (loop (i-1) $! (o+8))

--biggestIntegral x =fst $ B.foldr (\ i (a,c) -> (((fromIntegral i) `shiftL` c) + a,c+8)) (0,0) x 

infoOutput :: Options -> String -> expr -> expr 
infoOutput options s e = trace ("INFO:"++s) e
warningOutput :: Options -> String -> expr -> expr 
warningOutput options s e = trace ("WARNING:"++s) e



getbgword16 :: B.ByteString -> BitGet Word16
getbgword16 = getbgword

getbgword32 :: B.ByteString -> BitGet Word32
getbgword32 = getbgword

getbgword64 :: B.ByteString -> BitGet Word64
getbgword64 = getbgword

getbgword8 :: B.ByteString -> BitGet Word8
getbgword8 = getbgword


getbgword :: (Bits a, Integral a)=>B.ByteString -> BitGet a
getbgword = return . biggestIntegral


debug a = trace (show a) a 