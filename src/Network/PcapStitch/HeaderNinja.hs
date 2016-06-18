{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.PcapStitch.HeaderNinja (
  RequestFieldSize (..)
  ,Field(..)
  ,handleHeaderError
  ,getHeader
  ,R.Name(..)
  ,(.++.)
  ,headerTail
  ,addOptions
  ,addOptionsT
  ,(!!!)
  ,recordType
  ,wrapInData
  ,makeName
  ,arbitraryBits
  ,headerRecordShow
  ,F1,F2,F3,F4,F5,F6,F7,F8
  ,FSucc(FSucc)
  ,FZero(FZero)
)
where
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as G
import Data.Binary.Strict.BitGet
import qualified Data.Binary.Strict.BitGet as BG
import Data.Binary.Strict.Get
import Data.Binary.Strict.IncrementalGet
import Data.Word
import Data.Kind
import Data.Record
import qualified Data.Record as R
import Data.Record.Combinators (cat,Cat,withStyle,(!!!))
import Data.TypeFun
import Control.Monad
import Language.Haskell.TH
import qualified Language.Haskell.TH as TH
import Debug.Trace
import Data.Maybe

--------------------------------------------------------------------------------
--EXPERIMENTAL
-- | TODO

recordType ::  (ProperFormedRequestField t a) => Header (t a) rec ->  rec (Id KindStar) 
recordType = undefined


wrapInData :: String -> TH.Name -> Q [Dec]
wrapInData n a = do
  info <- reify a
  let typ = 
        case info of
          (VarI _ t _ _ ) -> t
          _ -> error "Bad type given to recordSignature"
  return $ [DataD [] (mkName n) [] [NormalC (mkName n) [(NotStrict,typ)]] []]
--  [DataD [] P [] [NormalC P [(NotStrict,ConT GHC.Types.Int)]] []]


test = [d| data Source = Source deriving (Show) |]

outputInfo :: TH.Name -> Q [Dec]
outputInfo a = do
  info <- reify a
  return $ trace (show info) []
--do
--  info <- reify nb

                  
{-
data Test = Test deriving (Show)
instance R.Name Test where
  name = Test

testsig = [d| data P = P Int |]
makeName2 = [d| instance R.Name Test where name = Test |] 
-}

data P = P deriving (Show)
makeName2 = [d| instance R.Name P where name = P |]

makeName :: String -> Q [Dec]
makeName n' = do
  let n = mkName n'
--  info <- reify n
--  let typ = 
--        case info of
--          (DataConI _ t _ _ ) -> t
--          _ -> error "Bad type given to makeName"
  let name = DataD [] n [] [NormalC n []] [mkName "Show"]     
  return $ name:[InstanceD [] (AppT (ConT (mkName "Name")) (ConT n)) [ValD (VarP (mkName "name")) (NormalB (ConE n)) []]]
--  return $ name:[InstanceD [] (AppT (ConT (mkName "Name")) typ) [ValD (VarP (mkName "name")) (NormalB (ConE n)) []]]

arbitraryBitsD = [d| t = FSucc $ FZero |]

arbitraryBits :: Int -> Q Exp
arbitraryBits b = do
  return $ fromJust $ foldl handle zero [1..b]
  where
    zero :: Maybe Exp
    zero = Just  $ ConE  $ mkName "FZero" 
    succ :: Maybe Exp
    succ = Just  $ ConE  $ mkName "FSucc" 
    op :: Exp
    op  = VarE $ mkName "$"
    addbit x = Just $ InfixE succ op x
    handle acc e = addbit acc

--[ValD (VarP t) (NormalB (InfixE (Just (ConE Network.PcapStitch.HeaderNinja.FSucc)) (VarE GHC.Base.$) (Just (InfixE (Just (ConE --Network.PcapStitch.HeaderNinja.FSucc)) (VarE GHC.Base.$) (Just (ConE Network.PcapStitch.HeaderNinja.FZero)))))) []]
--[ValD (VarP t) (NormalB (InfixE (Just (ConE Network.PcapStitch.HeaderNinja.FSucc)) (VarE GHC.Base.$) (Just (ConE Network.PcapStitch.HeaderNinja.FZero)))) []]

--------------------------------------------------------------------------------


{-|
  'FZero' and 'FSucc' are peano numbers that will be used
  to store the amount of Bits a field and ulimately a header
  has as a type signature.


-}
data FZero   = FZero
data FSucc a = FSucc a 


class Peano a where
  toN :: a -> Int

instance Peano FZero where
  toN _ = 0

instance (Peano a)=>Peano (FSucc a) where
  toN (FSucc a) =(+) 1 . toN $ a


{-|
  A number of synonyms for convience.
-}
type F1 = FSucc FZero
type F2 = FSucc F1
type F3 = FSucc F2
type F4 = FSucc F3
type F5 = FSucc F4
type F6 = FSucc F5
type F7 = FSucc F6
type F8 = FSucc F7



-- TODO - When RSFNBits is implemented this type family will be necessary.
type family Add a  b
type instance Add x  FZero     = x
--type instance Add FZero y      = y
type instance Add x (FSucc y)  = FSucc (Add x y)


-- | Adds One Bit 
type family Plus1 a
type instance Plus1  x = (FSucc x)

-- | Adds Two Bits
type family Plus2 a
type instance Plus2  x = (FSucc (FSucc x))

-- | Adds Eights Bits
type family Plus8 a
type instance Plus8 x = 
  (FSucc 
    (FSucc 
      (FSucc 
        (FSucc 
          (FSucc 
            (FSucc 
              (FSucc 
                (FSucc x))))))))

{-|
  'HTrue' and 'HFalse' are type based boolean values.  Right now they 
  are soley used to validate that a 'Header' is  byte aligned.
-}
data HTrue
data HFalse


{-|
  'ByteAligned''s purpose is self explanatory.  It checks that bytes are 
  aligned by subtracting 8. Until there are less than or equal to 8 bit
  where it will make a decision. Essentially preforms a 0 == x mod 8 
  test.
-}
type family ByteAligned a
type instance ByteAligned FZero = HTrue
type instance ByteAligned F1  = HFalse
type instance ByteAligned F2  = HFalse
type instance ByteAligned F3  = HFalse
type instance ByteAligned F4  = HFalse
type instance ByteAligned F5  = HFalse
type instance ByteAligned F6  = HFalse
type instance ByteAligned F7  = HFalse
type instance ByteAligned F8  = HTrue
type instance ByteAligned 
  (FSucc 
    (FSucc 
      (FSucc 
        (FSucc 
          (FSucc 
            (FSucc 
              (FSucc 
                (FSucc 
                  (FSucc x))))))))) = ByteAligned (FSucc x)



{-|
  'RequestFieldSize' is a way to describe a header by field size.
  
-}
data RequestFieldSize a where
  -- | 'RFSStop' is the end of a header and should always be aligned to the right.
  RFSStop  :: RequestFieldSize FZero
  -- | A one bit field.
  RFSBit   :: RequestFieldSize a -> RequestFieldSize (Plus1 a)
  -- | A two bit field
  RFS2Bit  :: RequestFieldSize a -> RequestFieldSize (Plus2 a) 
  -- | A three bit field
  RFS3Bit  :: RequestFieldSize a -> RequestFieldSize (Plus1 (Plus2 a))
  -- | A four bit field
  RFS4Bit  :: RequestFieldSize a -> RequestFieldSize (Plus2 (Plus2 a))
  -- | A five bit field
  RFS5Bit  :: RequestFieldSize a -> RequestFieldSize (Plus1 (Plus2 (Plus2 a)))
  -- | A six bit field
  RFS6Bit  :: RequestFieldSize a -> RequestFieldSize (Plus2 (Plus2 (Plus2 a)))
  -- | A seven bit field
  RFS7Bit  :: RequestFieldSize a 
           -> RequestFieldSize (Plus1 (Plus2 (Plus2 (Plus2 a))))
  -- | A byte sized field
  RFSWord8 :: RequestFieldSize a -> RequestFieldSize (Plus8 a)
  -- | A field sized to two bytes
  RFSWord16:: RequestFieldSize a -> RequestFieldSize (Plus8 (Plus8 a))
  -- | A field sized to four bytes
  RFSWord32:: RequestFieldSize a 
           -> RequestFieldSize (Plus8 (Plus8 (Plus8 (Plus8 a))))
  -- | A field sized to Six bytes
  RFSWord48:: RequestFieldSize a 
           -> RequestFieldSize (Plus8 (Plus8 (Plus8 (Plus8 (Plus8 (Plus8 a))))))
  -- | A field sized to eight bytes
  RFSWord64:: RequestFieldSize a 
           -> RequestFieldSize 
              (Plus8 (Plus8 (Plus8 (Plus8 (Plus8 (Plus8 (Plus8 (Plus8 a))))))))
  RFSNBits :: (Peano b)=> b -> RequestFieldSize a -> RequestFieldSize (Add a b)


{-|
  'ProperFormedRequestField' is used to enforce that all headers are byte 
  aligned at compile time.
-}
class ProperFormedRequestField t a where
  properlyFormed :: t a -> t a
instance (ByteAligned a~HTrue)=> ProperFormedRequestField RequestFieldSize a where
  properlyFormed = id

{-|
  'ImProperFormedRequestField' is the opposite of 'ProperlyFormedRequestField' 
  and used for testing purposes.
-}
instance ProperFormedRequestField RequestFieldSize FZero where
  properlyFormed = id
class ImProperFormedRequestField t a where
  improperlyFormed :: t a -> t a
instance  (ByteAligned a~HFalse)=>ImProperFormedRequestField RequestFieldSize a where
  improperlyFormed = id


{-| 
  Convert a single 'RequestFieldSize' and convert it to its size in
  bits.
-}
fieldSize :: RequestFieldSize a ->  Int
fieldSize (RFSStop) = 0
fieldSize (RFSBit x) = 1
fieldSize (RFS2Bit x) = 2
fieldSize (RFS3Bit x) = 3
fieldSize (RFS4Bit x) = 4
fieldSize (RFS5Bit x) = 5
fieldSize (RFS6Bit x) = 6
fieldSize (RFS7Bit x) = 7
fieldSize (RFSWord8 x) = 8
fieldSize (RFSWord16 x) = 16
fieldSize (RFSWord32 x) = 32
fieldSize (RFSWord48 x) = 48
fieldSize (RFSWord64 x) = 64
fieldSize (RFSNBits c x) = (toN c)

{-|
  'Field' is a single field in a header.
-}
data Field name size fieldtype name2 = 
  (R.Name name) => Field 
  {fieldname :: name 
  ,fieldconv :: ( B.ByteString -> BitGet fieldtype)
  ,fieldsize :: size}

{-|
  A 'Header' is built using '.++.' and a collection of 'Field's. Note
  that 'endField' must always be rightmost in this construction.
-}
data Header size rec = Header 
  {headersize :: size 
  ,headerrec  :: (BitGet (rec (Id KindStar)))}


class HeaderRecordShow a where
  headerRecordShow :: Int -> a -> ShowS

instance HeaderRecordShow (X style) where
  headerRecordShow _ _ = id

instance (Show (rec style), Show name, Show (App style sort)) =>
             HeaderRecordShow ((rec :& name ::: sort) style) where
  headerRecordShow enclPrec (rec :& field) = 
    showsPrec snocPrec rec          .
    showString " ["                 .
    showsPrec (succ snocPrec) field .
    showString "]"                 
    where
      snocPrec = 2

instance (Show name, Show (App style sort)) => HeaderRecordShow ((name ::: sort) style) where
  headerRecordShow enclPrec (name := val) =
    showsPrec (succ assignPrec) name .
    showString " -> "                .
    showsPrec (succ assignPrec) val 
    where
      assignPrec = 3


    
{-| 
  The type of the last record.
-}
data EndField = EndField deriving (Show)
instance R.Name EndField where
  name = EndField

{-|
  'endField' must always be rightmost in a string of '.++.' during 'Header' construction.
-}
headerTail = Header RFSStop $ return $ (X :& EndField := () `withStyle` Id KindStar)



addOptionsT :: (ProperFormedRequestField t a, Record KindStar rec,R.Name name) => Header (t a) rec -> name ->  Header (t a) (Cat ((:&) X (name ::: B.ByteString))  rec)
addOptionsT (Header ofs x) b = Header ofs $ liftM (cat (X :& b := B.empty `withStyle` Id KindStar)) x 

addOptions :: (Record KindStar rec,R.Name name) => rec  (Id KindStar) -> name ->  B.ByteString ->  (Cat ((:&) X (name ::: B.ByteString))  rec) (Id KindStar)
addOptions a b c = cat (X :& b := c `withStyle` Id KindStar) a


{-| 
  Operator used to build a header.
-}
(.++.) :: Record KindStar rec => 
       Field name (RequestFieldSize size -> RequestFieldSize nsize) fieldtype name2
       -> Header (RequestFieldSize size) rec
       -> Header (RequestFieldSize nsize) (Cat ((:&) X (name ::: fieldtype)) rec)
(.++.) (Field name conv nf ) (Header ofs x) = 
  Header nrfs $ genRFSParser
  where
    nrfs = nf ofs


    finish v = liftM (cat $ (X :& name := v) `withStyle` Id KindStar) x

    genRFSParser = getRightByteString (fieldSize nrfs) >>= conv >>= finish
infixr  8 .++.



{-|
  'getHeader' will trying to build the record. Left if an error occured.
  Right if the record was constructed successfully.
-}
getHeader :: (ProperFormedRequestField t a) => Header (t a) rec -> B.ByteString -> Either String (rec (Id KindStar),B.ByteString)
getHeader h = flip runBitGet (getHeaderI h)




handleHeaderError :: String -> Either String (rec (Id KindStar),B.ByteString) -> (rec (Id KindStar),B.ByteString)
handleHeaderError s (Left _)= error  s
handleHeaderError _ (Right r)= r

getHeaderI ::   (ProperFormedRequestField t a) => Header (t a) rec -> BitGet ((rec (Id KindStar)),B.ByteString)
getHeaderI (Header bf bg) = do
  let noop = properlyFormed bf
  rec <- bg
  r   <- BG.remaining
  rest<- getRightByteString r
  return $ (rec,rest)





test0 = properlyFormed $ RFS6Bit $ RFS2Bit $ RFSWord8 $ RFSStop
{-
-- | Test Cases to check that ByteAligned works. If you would like
-- to test the larger conditions make sure you only include one of them.
-- For some reason, if you include all of them GHC will try to acquire
-- all system memory.
test0 = improperlyFormed $ RFSBit $ RFSStop
test1 = properlyFormed $ RFSWord8 $ RFSStop
test2 = improperlyFormed $ RFSBit $ RFSStop
test3 = improperlyFormed $ RFS2Bit $ RFSStop
test4 = improperlyFormed $ RFS3Bit $ RFSStop
test5 = improperlyFormed $ RFS4Bit $ RFSStop
test6 = improperlyFormed $ RFS5Bit $ RFSStop
test7 = improperlyFormed $ RFS6Bit $ RFSStop
test8 = improperlyFormed $ RFS7Bit $ RFSStop
test9 = properlyFormed $ RFSWord16 $ RFSWord8 $ RFSStop 
test10 = properlyFormed $ RFSWord32 $ RFSWord16 $ RFSWord8 $ RFSStop 
test11 = improperlyFormed $ RFS3Bit $ RFSWord32 $ RFSWord16 $ RFSWord8 $ RFSStop
test12 = properlyFormed 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 $ RFSStop 
test13 = properlyFormed 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 $ RFSStop 
test14 = properlyFormed 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 $ RFSStop 

test15 = properlyFormed 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8$ RFSStop 
test16 = properlyFormed 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 $ RFSStop 

test17 = properlyFormed 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 $ RFSStop 
test18 = properlyFormed 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 $ RFSStop 
test19 = improperlyFormed 
       $ RFS6Bit
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 
       $ RFSWord64 $ RFSWord32 $ RFSWord16 $ RFSWord8 $ RFSStop 
-}

