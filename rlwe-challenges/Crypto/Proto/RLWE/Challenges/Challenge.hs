{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.RLWE.Challenges.Challenge (Challenge(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.RLWE.Challenges.Challenge.Params as Crypto.Proto.RLWE.Challenges.Challenge (Params)
import qualified Crypto.Proto.RLWE.Challenges.Challenge.Params as Crypto.Proto.RLWE.Challenges.Challenge.Params
       (Params(..), get'cparams, get'dparams, get'rparams)

data Challenge = Challenge{challengeID :: !(P'.Int32), numInstances :: !(P'.Int32), beaconEpoch :: !(P'.Int64),
                           beaconOffset :: !(P'.Int32), params :: P'.Maybe (Crypto.Proto.RLWE.Challenges.Challenge.Params)}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Challenge where
  mergeAppend (Challenge x'1 x'2 x'3 x'4 x'5) (Challenge y'1 y'2 y'3 y'4 y'5)
   = Challenge (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)

instance P'.Default Challenge where
  defaultValue = Challenge P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire Challenge where
  wireSize ft' self'@(Challenge x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeReq 1 5 x'2 + P'.wireSizeReq 1 3 x'3 + P'.wireSizeReq 1 5 x'4 +
             P'.wireSizeOpt 1 11 (Crypto.Proto.RLWE.Challenges.Challenge.Params.get'cparams Prelude'.=<< x'5)
             + P'.wireSizeOpt 1 11 (Crypto.Proto.RLWE.Challenges.Challenge.Params.get'dparams Prelude'.=<< x'5)
             + P'.wireSizeOpt 1 11 (Crypto.Proto.RLWE.Challenges.Challenge.Params.get'rparams Prelude'.=<< x'5))
  wirePut ft' self'@(Challenge x'1 x'2 x'3 x'4 x'5)
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
             P'.wirePutReq 24 3 x'3
             P'.wirePutReq 32 5 x'4
             P'.wirePutOpt 42 11 (Crypto.Proto.RLWE.Challenges.Challenge.Params.get'cparams Prelude'.=<< x'5)
             P'.wirePutOpt 50 11 (Crypto.Proto.RLWE.Challenges.Challenge.Params.get'dparams Prelude'.=<< x'5)
             P'.wirePutOpt 58 11 (Crypto.Proto.RLWE.Challenges.Challenge.Params.get'rparams Prelude'.=<< x'5)
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{challengeID = new'Field}) (P'.wireGet 5)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{numInstances = new'Field}) (P'.wireGet 5)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{beaconEpoch = new'Field}) (P'.wireGet 3)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{beaconOffset = new'Field}) (P'.wireGet 5)
             42 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{params =
                                P'.mergeAppend (params old'Self)
                                 (Prelude'.Just (Crypto.Proto.RLWE.Challenges.Challenge.Params.Cparams new'Field))})
                    (P'.wireGet 11)
             50 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{params =
                                P'.mergeAppend (params old'Self)
                                 (Prelude'.Just (Crypto.Proto.RLWE.Challenges.Challenge.Params.Dparams new'Field))})
                    (P'.wireGet 11)
             58 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{params =
                                P'.mergeAppend (params old'Self)
                                 (Prelude'.Just (Crypto.Proto.RLWE.Challenges.Challenge.Params.Rparams new'Field))})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Challenge) Challenge where
  getVal m' f' = f' m'

instance P'.GPB Challenge

instance P'.ReflectDescriptor Challenge where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 24, 32]) (P'.fromDistinctAscList [8, 16, 24, 32])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.Challenge\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\"], baseName = MName \"Challenge\"}, descFilePath = [\"Crypto\",\"Proto\",\"RLWE\",\"Challenges\",\"Challenge.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Challenge.challengeID\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\"], baseName' = FName \"challengeID\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Challenge.numInstances\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\"], baseName' = FName \"numInstances\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Challenge.beaconEpoch\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\"], baseName' = FName \"beaconEpoch\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Challenge.beaconOffset\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\"], baseName' = FName \"beaconOffset\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [OneofInfo {oneofName = ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.Challenge.params\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\"], baseName = MName \"Params\"}, oneofFName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Challenge.params\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\"], baseName' = FName \"params\", baseNamePrefix' = \"\"}, oneofFilePath = [\"Crypto\",\"Proto\",\"RLWE\",\"Challenges\",\"Challenge\",\"Params.hs\"], oneofFields = fromList [(ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.Challenge.params.cparams\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\",MName \"Params\"], baseName = MName \"Cparams\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Challenge.params.cparams\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\",MName \"Params\"], baseName' = FName \"cparams\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.ContParams\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\"], baseName = MName \"ContParams\"}), hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.Challenge.params.dparams\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\",MName \"Params\"], baseName = MName \"Dparams\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Challenge.params.dparams\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\",MName \"Params\"], baseName' = FName \"dparams\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.DiscParams\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\"], baseName = MName \"DiscParams\"}), hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.Challenge.params.rparams\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\",MName \"Params\"], baseName = MName \"Rparams\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Challenge.params.rparams\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Challenge\",MName \"Params\"], baseName' = FName \"rparams\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.RLWRParams\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\"], baseName = MName \"RLWRParams\"}), hsRawDefault = Nothing, hsDefault = Nothing})], oneofMakeLenses = False}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Challenge where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Challenge where
  textPut msg
   = do
       P'.tellT "challengeID" (challengeID msg)
       P'.tellT "numInstances" (numInstances msg)
       P'.tellT "beaconEpoch" (beaconEpoch msg)
       P'.tellT "beaconOffset" (beaconOffset msg)
       case (params msg) of
         Prelude'.Just (Crypto.Proto.RLWE.Challenges.Challenge.Params.Cparams cparams) -> P'.tellT "cparams" cparams
         Prelude'.Just (Crypto.Proto.RLWE.Challenges.Challenge.Params.Dparams dparams) -> P'.tellT "dparams" dparams
         Prelude'.Just (Crypto.Proto.RLWE.Challenges.Challenge.Params.Rparams rparams) -> P'.tellT "rparams" rparams
         Prelude'.Nothing -> Prelude'.return ()
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'challengeID, parse'numInstances, parse'beaconEpoch, parse'beaconOffset, parse'params])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'challengeID
         = P'.try
            (do
               v <- P'.getT "challengeID"
               Prelude'.return (\ o -> o{challengeID = v}))
        parse'numInstances
         = P'.try
            (do
               v <- P'.getT "numInstances"
               Prelude'.return (\ o -> o{numInstances = v}))
        parse'beaconEpoch
         = P'.try
            (do
               v <- P'.getT "beaconEpoch"
               Prelude'.return (\ o -> o{beaconEpoch = v}))
        parse'beaconOffset
         = P'.try
            (do
               v <- P'.getT "beaconOffset"
               Prelude'.return (\ o -> o{beaconOffset = v}))
        parse'params = P'.try (P'.choice [parse'cparams, parse'dparams, parse'rparams])
          where
              parse'cparams
               = P'.try
                  (do
                     v <- P'.getT "cparams"
                     Prelude'.return (\ s -> s{params = Prelude'.Just (Crypto.Proto.RLWE.Challenges.Challenge.Params.Cparams v)}))
              parse'dparams
               = P'.try
                  (do
                     v <- P'.getT "dparams"
                     Prelude'.return (\ s -> s{params = Prelude'.Just (Crypto.Proto.RLWE.Challenges.Challenge.Params.Dparams v)}))
              parse'rparams
               = P'.try
                  (do
                     v <- P'.getT "rparams"
                     Prelude'.return (\ s -> s{params = Prelude'.Just (Crypto.Proto.RLWE.Challenges.Challenge.Params.Rparams v)}))