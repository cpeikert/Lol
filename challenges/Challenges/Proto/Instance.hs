{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Challenges.Proto.Instance (Instance(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Challenges.Proto.InstType as Lwe.Instance (InstType)
import qualified Challenges.Proto.InstType as Lwe.Instance.InstType (InstType(..), get'clweinst, get'dlweinst, get'lwrinst)

data Instance = Instance{instType :: P'.Maybe (Lwe.Instance.InstType)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable Instance where
  mergeAppend (Instance x'1) (Instance y'1) = Instance (P'.mergeAppend x'1 y'1)

instance P'.Default Instance where
  defaultValue = Instance P'.defaultValue

instance P'.Wire Instance where
  wireSize ft' self'@(Instance x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 11 (Lwe.Instance.InstType.get'clweinst Prelude'.=<< x'1) +
             P'.wireSizeOpt 1 11 (Lwe.Instance.InstType.get'dlweinst Prelude'.=<< x'1)
             + P'.wireSizeOpt 1 11 (Lwe.Instance.InstType.get'lwrinst Prelude'.=<< x'1))
  wirePut ft' self'@(Instance x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 11 (Lwe.Instance.InstType.get'clweinst Prelude'.=<< x'1)
             P'.wirePutOpt 18 11 (Lwe.Instance.InstType.get'dlweinst Prelude'.=<< x'1)
             P'.wirePutOpt 26 11 (Lwe.Instance.InstType.get'lwrinst Prelude'.=<< x'1)
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{instType =
                                P'.mergeAppend (instType old'Self) (Prelude'.Just (Lwe.Instance.InstType.Clweinst new'Field))})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{instType =
                                P'.mergeAppend (instType old'Self) (Prelude'.Just (Lwe.Instance.InstType.Dlweinst new'Field))})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{instType =
                                P'.mergeAppend (instType old'Self) (Prelude'.Just (Lwe.Instance.InstType.Lwrinst new'Field))})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Instance) Instance where
  getVal m' f' = f' m'

instance P'.GPB Instance

instance P'.ReflectDescriptor Instance where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Lwe.Instance\", haskellPrefix = [], parentModule = [MName \"Lwe\"], baseName = MName \"Instance\"}, descFilePath = [\"Lwe\",\"Instance.hs\"], isGroup = False, fields = fromList [], descOneofs = fromList [OneofInfo {oneofName = ProtoName {protobufName = FIName \".Lwe.Instance.instType\", haskellPrefix = [], parentModule = [MName \"Lwe\",MName \"Instance\"], baseName = MName \"InstType\"}, oneofFName = ProtoFName {protobufName' = FIName \".Lwe.Instance.instType\", haskellPrefix' = [], parentModule' = [MName \"Lwe\",MName \"Instance\"], baseName' = FName \"instType\", baseNamePrefix' = \"\"}, oneofFilePath = [\"Lwe\",\"Instance\",\"InstType.hs\"], oneofFields = fromList [(ProtoName {protobufName = FIName \".Lwe.Instance.instType.clweinst\", haskellPrefix = [], parentModule = [MName \"Lwe\",MName \"Instance\",MName \"InstType\"], baseName = MName \"Clweinst\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lwe.Instance.instType.clweinst\", haskellPrefix' = [], parentModule' = [MName \"Lwe\",MName \"Instance\",MName \"InstType\"], baseName' = FName \"clweinst\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lwe.ContLWEInstance\", haskellPrefix = [], parentModule = [MName \"Lwe\"], baseName = MName \"ContLWEInstance\"}), hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {protobufName = FIName \".Lwe.Instance.instType.dlweinst\", haskellPrefix = [], parentModule = [MName \"Lwe\",MName \"Instance\",MName \"InstType\"], baseName = MName \"Dlweinst\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lwe.Instance.instType.dlweinst\", haskellPrefix' = [], parentModule' = [MName \"Lwe\",MName \"Instance\",MName \"InstType\"], baseName' = FName \"dlweinst\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lwe.DiscLWEInstance\", haskellPrefix = [], parentModule = [MName \"Lwe\"], baseName = MName \"DiscLWEInstance\"}), hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {protobufName = FIName \".Lwe.Instance.instType.lwrinst\", haskellPrefix = [], parentModule = [MName \"Lwe\",MName \"Instance\",MName \"InstType\"], baseName = MName \"Lwrinst\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lwe.Instance.instType.lwrinst\", haskellPrefix' = [], parentModule' = [MName \"Lwe\",MName \"Instance\",MName \"InstType\"], baseName' = FName \"lwrinst\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lwe.LWRInstance\", haskellPrefix = [], parentModule = [MName \"Lwe\"], baseName = MName \"LWRInstance\"}), hsRawDefault = Nothing, hsDefault = Nothing})], oneofMakeLenses = False}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Instance where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Instance where
  textPut msg
   = do
       case (instType msg) of
         Prelude'.Just (Lwe.Instance.InstType.Clweinst clweinst) -> P'.tellT "clweinst" clweinst
         Prelude'.Just (Lwe.Instance.InstType.Dlweinst dlweinst) -> P'.tellT "dlweinst" dlweinst
         Prelude'.Just (Lwe.Instance.InstType.Lwrinst lwrinst) -> P'.tellT "lwrinst" lwrinst
         Prelude'.Nothing -> Prelude'.return ()
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'instType]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'instType = P'.try (P'.choice [parse'clweinst, parse'dlweinst, parse'lwrinst])
          where
              parse'clweinst
               = P'.try
                  (do
                     v <- P'.getT "clweinst"
                     Prelude'.return (\ s -> s{instType = Prelude'.Just (Lwe.Instance.InstType.Clweinst v)}))
              parse'dlweinst
               = P'.try
                  (do
                     v <- P'.getT "dlweinst"
                     Prelude'.return (\ s -> s{instType = Prelude'.Just (Lwe.Instance.InstType.Dlweinst v)}))
              parse'lwrinst
               = P'.try
                  (do
                     v <- P'.getT "lwrinst"
                     Prelude'.return (\ s -> s{instType = Prelude'.Just (Lwe.Instance.InstType.Lwrinst v)}))