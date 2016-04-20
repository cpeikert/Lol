{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Challenges.RLWE.Proto.RLWE.Instance (Instance(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Challenges.RLWE.Proto.RLWE.Instance.InstType as RLWE.Instance (InstType)
import qualified Crypto.Challenges.RLWE.Proto.RLWE.Instance.InstType as RLWE.Instance.InstType
       (InstType(..), get'instCont, get'instDisc, get'instRLWR)

data Instance = Instance{instType :: P'.Maybe (RLWE.Instance.InstType)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

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
         = (P'.wireSizeOpt 1 11 (RLWE.Instance.InstType.get'instCont Prelude'.=<< x'1) +
             P'.wireSizeOpt 1 11 (RLWE.Instance.InstType.get'instDisc Prelude'.=<< x'1)
             + P'.wireSizeOpt 1 11 (RLWE.Instance.InstType.get'instRLWR Prelude'.=<< x'1))
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
             P'.wirePutOpt 10 11 (RLWE.Instance.InstType.get'instCont Prelude'.=<< x'1)
             P'.wirePutOpt 18 11 (RLWE.Instance.InstType.get'instDisc Prelude'.=<< x'1)
             P'.wirePutOpt 26 11 (RLWE.Instance.InstType.get'instRLWR Prelude'.=<< x'1)
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
                                P'.mergeAppend (instType old'Self) (Prelude'.Just (RLWE.Instance.InstType.InstCont new'Field))})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{instType =
                                P'.mergeAppend (instType old'Self) (Prelude'.Just (RLWE.Instance.InstType.InstDisc new'Field))})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{instType =
                                P'.mergeAppend (instType old'Self) (Prelude'.Just (RLWE.Instance.InstType.InstRLWR new'Field))})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Instance) Instance where
  getVal m' f' = f' m'

instance P'.GPB Instance

instance P'.ReflectDescriptor Instance where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RLWE.Instance\", haskellPrefix = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule = [MName \"RLWE\"], baseName = MName \"Instance\"}, descFilePath = [\"Crypto\",\"Challenges\",\"RLWE\",\"Proto\",\"RLWE\",\"Instance.hs\"], isGroup = False, fields = fromList [], descOneofs = fromList [OneofInfo {oneofName = ProtoName {protobufName = FIName \".RLWE.Instance.instType\", haskellPrefix = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule = [MName \"RLWE\",MName \"Instance\"], baseName = MName \"InstType\"}, oneofFName = ProtoFName {protobufName' = FIName \".RLWE.Instance.instType\", haskellPrefix' = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"Instance\"], baseName' = FName \"instType\", baseNamePrefix' = \"\"}, oneofFilePath = [\"Crypto\",\"Challenges\",\"RLWE\",\"Proto\",\"RLWE\",\"Instance\",\"InstType.hs\"], oneofFields = fromList [(ProtoName {protobufName = FIName \".RLWE.Instance.instType.instCont\", haskellPrefix = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule = [MName \"RLWE\",MName \"Instance\",MName \"InstType\"], baseName = MName \"InstCont\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.Instance.instType.instCont\", haskellPrefix' = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"Instance\",MName \"InstType\"], baseName' = FName \"instCont\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RLWE.InstanceCont\", haskellPrefix = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule = [MName \"RLWE\"], baseName = MName \"InstanceCont\"}), hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {protobufName = FIName \".RLWE.Instance.instType.instDisc\", haskellPrefix = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule = [MName \"RLWE\",MName \"Instance\",MName \"InstType\"], baseName = MName \"InstDisc\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.Instance.instType.instDisc\", haskellPrefix' = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"Instance\",MName \"InstType\"], baseName' = FName \"instDisc\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RLWE.InstanceDisc\", haskellPrefix = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule = [MName \"RLWE\"], baseName = MName \"InstanceDisc\"}), hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {protobufName = FIName \".RLWE.Instance.instType.instRLWR\", haskellPrefix = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule = [MName \"RLWE\",MName \"Instance\",MName \"InstType\"], baseName = MName \"InstRLWR\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.Instance.instType.instRLWR\", haskellPrefix' = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"Instance\",MName \"InstType\"], baseName' = FName \"instRLWR\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RLWE.InstanceRLWR\", haskellPrefix = [MName \"Crypto\",MName \"Challenges\",MName \"RLWE\",MName \"Proto\"], parentModule = [MName \"RLWE\"], baseName = MName \"InstanceRLWR\"}), hsRawDefault = Nothing, hsDefault = Nothing})], oneofMakeLenses = False}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Instance where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Instance where
  textPut msg
   = do
       case (instType msg) of
         Prelude'.Just (RLWE.Instance.InstType.InstCont instCont) -> P'.tellT "instCont" instCont
         Prelude'.Just (RLWE.Instance.InstType.InstDisc instDisc) -> P'.tellT "instDisc" instDisc
         Prelude'.Just (RLWE.Instance.InstType.InstRLWR instRLWR) -> P'.tellT "instRLWR" instRLWR
         Prelude'.Nothing -> Prelude'.return ()
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'instType]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'instType = P'.try (P'.choice [parse'instCont, parse'instDisc, parse'instRLWR])
          where
              parse'instCont
               = P'.try
                  (do
                     v <- P'.getT "instCont"
                     Prelude'.return (\ s -> s{instType = Prelude'.Just (RLWE.Instance.InstType.InstCont v)}))
              parse'instDisc
               = P'.try
                  (do
                     v <- P'.getT "instDisc"
                     Prelude'.return (\ s -> s{instType = Prelude'.Just (RLWE.Instance.InstType.InstDisc v)}))
              parse'instRLWR
               = P'.try
                  (do
                     v <- P'.getT "instRLWR"
                     Prelude'.return (\ s -> s{instType = Prelude'.Just (RLWE.Instance.InstType.InstRLWR v)}))