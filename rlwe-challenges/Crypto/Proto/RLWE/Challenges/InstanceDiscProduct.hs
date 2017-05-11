{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.RLWE.Challenges.InstanceDiscProduct (InstanceDiscProduct(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.Crypto.Proto.RLWE.SampleDiscProduct as Crypto.Proto.RLWE (SampleDiscProduct)
import qualified Crypto.Proto.RLWE.Challenges.DiscParams as Crypto.Proto.RLWE.Challenges (DiscParams)

data InstanceDiscProduct = InstanceDiscProduct{challengeID :: !(P'.Int32), instanceID :: !(P'.Int32),
                                               params :: !(Crypto.Proto.RLWE.Challenges.DiscParams),
                                               samples :: !(P'.Seq Crypto.Proto.RLWE.SampleDiscProduct)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable InstanceDiscProduct where
  mergeAppend (InstanceDiscProduct x'1 x'2 x'3 x'4) (InstanceDiscProduct y'1 y'2 y'3 y'4)
   = InstanceDiscProduct (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default InstanceDiscProduct where
  defaultValue = InstanceDiscProduct P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire InstanceDiscProduct where
  wireSize ft' self'@(InstanceDiscProduct x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeReq 1 5 x'2 + P'.wireSizeReq 1 11 x'3 + P'.wireSizeRep 1 11 x'4)
  wirePut ft' self'@(InstanceDiscProduct x'1 x'2 x'3 x'4)
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
             P'.wirePutReq 16 5 x'2
             P'.wirePutReq 26 11 x'3
             P'.wirePutRep 34 11 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{challengeID = new'Field}) (P'.wireGet 5)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{instanceID = new'Field}) (P'.wireGet 5)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{params = P'.mergeAppend (params old'Self) (new'Field)}) (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{samples = P'.append (samples old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> InstanceDiscProduct) InstanceDiscProduct where
  getVal m' f' = f' m'

instance P'.GPB InstanceDiscProduct

instance P'.ReflectDescriptor InstanceDiscProduct where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 26]) (P'.fromDistinctAscList [8, 16, 26, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.InstanceDiscProduct\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\"], baseName = MName \"InstanceDiscProduct\"}, descFilePath = [\"Crypto\",\"Proto\",\"RLWE\",\"Challenges\",\"InstanceDiscProduct.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.InstanceDiscProduct.challengeID\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"InstanceDiscProduct\"], baseName' = FName \"challengeID\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.InstanceDiscProduct.instanceID\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"InstanceDiscProduct\"], baseName' = FName \"instanceID\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.InstanceDiscProduct.params\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"InstanceDiscProduct\"], baseName' = FName \"params\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.DiscParams\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\"], baseName = MName \"DiscParams\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.InstanceDiscProduct.samples\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"InstanceDiscProduct\"], baseName' = FName \"samples\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".crypto.proto.RLWE.SampleDiscProduct\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\"], baseName = MName \"SampleDiscProduct\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType InstanceDiscProduct where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg InstanceDiscProduct where
  textPut msg
   = do
       P'.tellT "challengeID" (challengeID msg)
       P'.tellT "instanceID" (instanceID msg)
       P'.tellT "params" (params msg)
       P'.tellT "samples" (samples msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'challengeID, parse'instanceID, parse'params, parse'samples]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'challengeID
         = P'.try
            (do
               v <- P'.getT "challengeID"
               Prelude'.return (\ o -> o{challengeID = v}))
        parse'instanceID
         = P'.try
            (do
               v <- P'.getT "instanceID"
               Prelude'.return (\ o -> o{instanceID = v}))
        parse'params
         = P'.try
            (do
               v <- P'.getT "params"
               Prelude'.return (\ o -> o{params = v}))
        parse'samples
         = P'.try
            (do
               v <- P'.getT "samples"
               Prelude'.return (\ o -> o{samples = P'.append (samples o) v}))