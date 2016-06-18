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


module Network.PcapStitch.Header.Header (
       Header(Empty,Error,Header),
       runHeader,
       liftTupleToHeaderTuple
) where
import Network.PcapStitch.Address
import Network.PcapStitch.Classes



data Header a = Header {-# UNPACK #-} !a
              | Empty
              | Error

runHeader :: Header a -> a
runHeader (Header a) = a
runHeader x = error $ "runHeader: Unsupported "

instance (Show a)=>Show (Header a) where
         showsPrec p (Header x) = showsPrec p x
         showsPrec p Empty = showsPrec p "Empty"
         showsPrec p Error= showsPrec p "Error"

liftTupleToHeaderTuple :: (Show x) => (x,y) -> (Header x, y)
liftTupleToHeaderTuple = uncurry (\ a b -> (Header a, b))

instance (HasPayloadType a)=>HasPayloadType (Header a) where
         getPayloadType (Header x) = getPayloadType x

instance (HasSource a)=>HasSource (Header a) where
  getSource (Header a) = getSource a
  getSource _ = NoAddress

instance (HasDestination a)=>HasDestination (Header a) where
  getDestination (Header a) = getDestination a
  getDestination _ = NoAddress



{-
instance (Matchable a)=>Matchable (Header a) where
  same (Header a) (Header b) = same a b
  same _ _ = False
-}



