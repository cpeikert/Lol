{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.RLWE.Challenges.DRBGSeed (DRBGSeed(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data DRBGSeed = DRBGSeed{seedBytes :: !(P'.ByteString), seedLen :: !(P'.Int32)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable DRBGSeed where
  mergeAppend (DRBGSeed x'1 x'2) (DRBGSeed y'1 y'2) = DRBGSeed (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default DRBGSeed where
  defaultValue = DRBGSeed P'.defaultValue P'.defaultValue

instance P'.Wire DRBGSeed where
  wireSize ft' self'@(DRBGSeed x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 12 x'1 + P'.wireSizeReq 1 5 x'2)
  wirePut ft' self'@(DRBGSeed x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 12 x'1
             P'.wirePutReq 16 5 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{seedBytes = new'Field}) (P'.wireGet 12)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{seedLen = new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> DRBGSeed) DRBGSeed where
  getVal m' f' = f' m'

instance P'.GPB DRBGSeed

instance P'.ReflectDescriptor DRBGSeed where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 16]) (P'.fromDistinctAscList [10, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Challenges.DRBGSeed\", haskellPrefix = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\"], parentModule = [MName \"Challenges\"], baseName = MName \"DRBGSeed\"}, descFilePath = [\"Crypto\",\"Proto\",\"RLWE\",\"Challenges\",\"DRBGSeed.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Challenges.DRBGSeed.seedBytes\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\"], parentModule' = [MName \"Challenges\",MName \"DRBGSeed\"], baseName' = FName \"seedBytes\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Challenges.DRBGSeed.seedLen\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\"], parentModule' = [MName \"Challenges\",MName \"DRBGSeed\"], baseName' = FName \"seedLen\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType DRBGSeed where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg DRBGSeed where
  textPut msg
   = do
       P'.tellT "seedBytes" (seedBytes msg)
       P'.tellT "seedLen" (seedLen msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'seedBytes, parse'seedLen]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'seedBytes
         = P'.try
            (do
               v <- P'.getT "seedBytes"
               Prelude'.return (\ o -> o{seedBytes = v}))
        parse'seedLen
         = P'.try
            (do
               v <- P'.getT "seedLen"
               Prelude'.return (\ o -> o{seedLen = v}))