{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.RLWE.Challenges.RLWRParams (RLWRParams(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data RLWRParams = RLWRParams{m :: !(P'.Int32), q :: !(P'.Int64), p :: !(P'.Int64), numSamples :: !(P'.Int32)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable RLWRParams where
  mergeAppend (RLWRParams x'1 x'2 x'3 x'4) (RLWRParams y'1 y'2 y'3 y'4)
   = RLWRParams (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default RLWRParams where
  defaultValue = RLWRParams P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire RLWRParams where
  wireSize ft' self'@(RLWRParams x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeReq 1 3 x'2 + P'.wireSizeReq 1 3 x'3 + P'.wireSizeReq 1 5 x'4)
  wirePut ft' self'@(RLWRParams x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 5 x'1
             P'.wirePutReq 16 3 x'2
             P'.wirePutReq 24 3 x'3
             P'.wirePutReq 32 5 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{m = new'Field}) (P'.wireGet 5)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{q = new'Field}) (P'.wireGet 3)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{p = new'Field}) (P'.wireGet 3)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{numSamples = new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> RLWRParams) RLWRParams where
  getVal m' f' = f' m'

instance P'.GPB RLWRParams

instance P'.ReflectDescriptor RLWRParams where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 24, 32]) (P'.fromDistinctAscList [8, 16, 24, 32])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.RLWRParams\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\"], baseName = MName \"RLWRParams\"}, descFilePath = [\"Crypto\",\"Proto\",\"RLWE\",\"Challenges\",\"RLWRParams.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.RLWRParams.m\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"RLWRParams\"], baseName' = FName \"m\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.RLWRParams.q\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"RLWRParams\"], baseName' = FName \"q\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.RLWRParams.p\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"RLWRParams\"], baseName' = FName \"p\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.RLWRParams.numSamples\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"RLWRParams\"], baseName' = FName \"numSamples\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType RLWRParams where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg RLWRParams where
  textPut msg
   = do
       P'.tellT "m" (m msg)
       P'.tellT "q" (q msg)
       P'.tellT "p" (p msg)
       P'.tellT "numSamples" (numSamples msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'm, parse'q, parse'p, parse'numSamples]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'm
         = P'.try
            (do
               v <- P'.getT "m"
               Prelude'.return (\ o -> o{m = v}))
        parse'q
         = P'.try
            (do
               v <- P'.getT "q"
               Prelude'.return (\ o -> o{q = v}))
        parse'p
         = P'.try
            (do
               v <- P'.getT "p"
               Prelude'.return (\ o -> o{p = v}))
        parse'numSamples
         = P'.try
            (do
               v <- P'.getT "numSamples"
               Prelude'.return (\ o -> o{numSamples = v}))