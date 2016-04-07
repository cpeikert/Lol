{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Lol.Types.Proto.TensorMsg (TensorMsg(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Lol.Types.Proto.Coeffs as Lol.TensorMsg (Coeffs)
import qualified Crypto.Lol.Types.Proto.Coeffs as Lol.TensorMsg.Coeffs (Coeffs(..), get'zqs, get'rqs)

data TensorMsg = TensorMsg{m :: !(P'.Word32), coeffs :: P'.Maybe (Lol.TensorMsg.Coeffs)}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable TensorMsg where
  mergeAppend (TensorMsg x'1 x'2) (TensorMsg y'1 y'2) = TensorMsg (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default TensorMsg where
  defaultValue = TensorMsg P'.defaultValue P'.defaultValue

instance P'.Wire TensorMsg where
  wireSize ft' self'@(TensorMsg x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 13 x'1 + P'.wireSizeOpt 1 11 (Lol.TensorMsg.Coeffs.get'zqs Prelude'.=<< x'2) +
             P'.wireSizeOpt 1 11 (Lol.TensorMsg.Coeffs.get'rqs Prelude'.=<< x'2))
  wirePut ft' self'@(TensorMsg x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 13 x'1
             P'.wirePutOpt 18 11 (Lol.TensorMsg.Coeffs.get'zqs Prelude'.=<< x'2)
             P'.wirePutOpt 26 11 (Lol.TensorMsg.Coeffs.get'rqs Prelude'.=<< x'2)
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{m = new'Field}) (P'.wireGet 13)
             18 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{coeffs = P'.mergeAppend (coeffs old'Self) (Prelude'.Just (Lol.TensorMsg.Coeffs.Zqs new'Field))})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{coeffs = P'.mergeAppend (coeffs old'Self) (Prelude'.Just (Lol.TensorMsg.Coeffs.Rqs new'Field))})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TensorMsg) TensorMsg where
  getVal m' f' = f' m'

instance P'.GPB TensorMsg

instance P'.ReflectDescriptor TensorMsg where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8]) (P'.fromDistinctAscList [8])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Lol.TensorMsg\", haskellPrefix = [], parentModule = [MName \"Lol\"], baseName = MName \"TensorMsg\"}, descFilePath = [\"Lol\",\"TensorMsg.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.TensorMsg.m\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"TensorMsg\"], baseName' = FName \"m\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [OneofInfo {oneofName = ProtoName {protobufName = FIName \".Lol.TensorMsg.coeffs\", haskellPrefix = [], parentModule = [MName \"Lol\",MName \"TensorMsg\"], baseName = MName \"Coeffs\"}, oneofFName = ProtoFName {protobufName' = FIName \".Lol.TensorMsg.coeffs\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"TensorMsg\"], baseName' = FName \"coeffs\", baseNamePrefix' = \"\"}, oneofFilePath = [\"Lol\",\"TensorMsg\",\"Coeffs.hs\"], oneofFields = fromList [(ProtoName {protobufName = FIName \".Lol.TensorMsg.coeffs.zqs\", haskellPrefix = [], parentModule = [MName \"Lol\",MName \"TensorMsg\",MName \"Coeffs\"], baseName = MName \"Zqs\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.TensorMsg.coeffs.zqs\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"TensorMsg\",MName \"Coeffs\"], baseName' = FName \"zqs\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lol.Int64List\", haskellPrefix = [], parentModule = [MName \"Lol\"], baseName = MName \"Int64List\"}), hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {protobufName = FIName \".Lol.TensorMsg.coeffs.rqs\", haskellPrefix = [], parentModule = [MName \"Lol\",MName \"TensorMsg\",MName \"Coeffs\"], baseName = MName \"Rqs\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.TensorMsg.coeffs.rqs\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"TensorMsg\",MName \"Coeffs\"], baseName' = FName \"rqs\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lol.DoubleList\", haskellPrefix = [], parentModule = [MName \"Lol\"], baseName = MName \"DoubleList\"}), hsRawDefault = Nothing, hsDefault = Nothing})], oneofMakeLenses = False}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType TensorMsg where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TensorMsg where
  textPut msg
   = do
       P'.tellT "m" (m msg)
       case (coeffs msg) of
         Prelude'.Just (Lol.TensorMsg.Coeffs.Zqs zqs) -> P'.tellT "zqs" zqs
         Prelude'.Just (Lol.TensorMsg.Coeffs.Rqs rqs) -> P'.tellT "rqs" rqs
         Prelude'.Nothing -> Prelude'.return ()
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'm, parse'coeffs]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'm
         = P'.try
            (do
               v <- P'.getT "m"
               Prelude'.return (\ o -> o{m = v}))
        parse'coeffs = P'.try (P'.choice [parse'zqs, parse'rqs])
          where
              parse'zqs
               = P'.try
                  (do
                     v <- P'.getT "zqs"
                     Prelude'.return (\ s -> s{coeffs = Prelude'.Just (Lol.TensorMsg.Coeffs.Zqs v)}))
              parse'rqs
               = P'.try
                  (do
                     v <- P'.getT "rqs"
                     Prelude'.return (\ s -> s{coeffs = Prelude'.Just (Lol.TensorMsg.Coeffs.Rqs v)}))