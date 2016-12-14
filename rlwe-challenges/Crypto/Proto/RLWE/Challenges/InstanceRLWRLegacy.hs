{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.RLWE.Challenges.InstanceRLWRLegacy (InstanceRLWRLegacy(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.RLWE.Challenges.RLWRParams as Challenges (RLWRParams)
import qualified Crypto.Proto.RLWE.SampleRLWRLegacy as RLWE (SampleRLWRLegacy)

data InstanceRLWRLegacy = InstanceRLWRLegacy{challengeID :: !(P'.Int32), instanceID :: !(P'.Int32),
                                             params :: !(Challenges.RLWRParams), samples :: !(P'.Seq RLWE.SampleRLWRLegacy)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable InstanceRLWRLegacy where
  mergeAppend (InstanceRLWRLegacy x'1 x'2 x'3 x'4) (InstanceRLWRLegacy y'1 y'2 y'3 y'4)
   = InstanceRLWRLegacy (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default InstanceRLWRLegacy where
  defaultValue = InstanceRLWRLegacy P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire InstanceRLWRLegacy where
  wireSize ft' self'@(InstanceRLWRLegacy x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeReq 1 5 x'2 + P'.wireSizeReq 1 11 x'3 + P'.wireSizeRep 1 11 x'4)
  wirePut ft' self'@(InstanceRLWRLegacy x'1 x'2 x'3 x'4)
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

instance P'.MessageAPI msg' (msg' -> InstanceRLWRLegacy) InstanceRLWRLegacy where
  getVal m' f' = f' m'

instance P'.GPB InstanceRLWRLegacy

instance P'.ReflectDescriptor InstanceRLWRLegacy where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 26]) (P'.fromDistinctAscList [8, 16, 26, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Challenges.InstanceRLWRLegacy\", haskellPrefix = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\"], parentModule = [MName \"Challenges\"], baseName = MName \"InstanceRLWRLegacy\"}, descFilePath = [\"Crypto\",\"Proto\",\"RLWE\",\"Challenges\",\"InstanceRLWRLegacy.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Challenges.InstanceRLWRLegacy.challengeID\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\"], parentModule' = [MName \"Challenges\",MName \"InstanceRLWRLegacy\"], baseName' = FName \"challengeID\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Challenges.InstanceRLWRLegacy.instanceID\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\"], parentModule' = [MName \"Challenges\",MName \"InstanceRLWRLegacy\"], baseName' = FName \"instanceID\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Challenges.InstanceRLWRLegacy.params\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\"], parentModule' = [MName \"Challenges\",MName \"InstanceRLWRLegacy\"], baseName' = FName \"params\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Challenges.RLWRParams\", haskellPrefix = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\"], parentModule = [MName \"Challenges\"], baseName = MName \"RLWRParams\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Challenges.InstanceRLWRLegacy.samples\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\"], parentModule' = [MName \"Challenges\",MName \"InstanceRLWRLegacy\"], baseName' = FName \"samples\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RLWE.SampleRLWRLegacy\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"RLWE\"], baseName = MName \"SampleRLWRLegacy\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType InstanceRLWRLegacy where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg InstanceRLWRLegacy where
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