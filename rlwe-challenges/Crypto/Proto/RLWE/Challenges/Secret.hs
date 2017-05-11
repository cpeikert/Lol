{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.RLWE.Challenges.Secret (Secret(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.Crypto.Proto.Lol.Rq as Crypto.Proto.Lol (Rq)

data Secret = Secret{challengeID :: !(P'.Int32), instanceID :: !(P'.Int32), m :: !(P'.Int32), q :: !(P'.Int64),
                     seed :: !(P'.ByteString), s :: !(Crypto.Proto.Lol.Rq)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Secret where
  mergeAppend (Secret x'1 x'2 x'3 x'4 x'5 x'6) (Secret y'1 y'2 y'3 y'4 y'5 y'6)
   = Secret (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)

instance P'.Default Secret where
  defaultValue = Secret P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire Secret where
  wireSize ft' self'@(Secret x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeReq 1 5 x'2 + P'.wireSizeReq 1 5 x'3 + P'.wireSizeReq 1 3 x'4 +
             P'.wireSizeReq 1 12 x'5
             + P'.wireSizeReq 1 11 x'6)
  wirePut ft' self'@(Secret x'1 x'2 x'3 x'4 x'5 x'6)
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
             P'.wirePutReq 24 5 x'3
             P'.wirePutReq 32 3 x'4
             P'.wirePutReq 42 12 x'5
             P'.wirePutReq 50 11 x'6
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
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{m = new'Field}) (P'.wireGet 5)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{q = new'Field}) (P'.wireGet 3)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{seed = new'Field}) (P'.wireGet 12)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{s = P'.mergeAppend (s old'Self) (new'Field)}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Secret) Secret where
  getVal m' f' = f' m'

instance P'.GPB Secret

instance P'.ReflectDescriptor Secret where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 24, 32, 42, 50]) (P'.fromDistinctAscList [8, 16, 24, 32, 42, 50])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".crypto.proto.RLWE.challenges.Secret\", haskellPrefix = [], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\"], baseName = MName \"Secret\"}, descFilePath = [\"Crypto\",\"Proto\",\"RLWE\",\"Challenges\",\"Secret.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Secret.challengeID\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Secret\"], baseName' = FName \"challengeID\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Secret.instanceID\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Secret\"], baseName' = FName \"instanceID\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Secret.m\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Secret\"], baseName' = FName \"m\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Secret.q\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Secret\"], baseName' = FName \"q\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Secret.seed\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Secret\"], baseName' = FName \"seed\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".crypto.proto.RLWE.challenges.Secret.s\", haskellPrefix' = [], parentModule' = [MName \"Crypto\",MName \"Proto\",MName \"RLWE\",MName \"Challenges\",MName \"Secret\"], baseName' = FName \"s\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".crypto.proto.lol.Rq\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Crypto\",MName \"Proto\",MName \"Lol\"], baseName = MName \"Rq\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Secret where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Secret where
  textPut msg
   = do
       P'.tellT "challengeID" (challengeID msg)
       P'.tellT "instanceID" (instanceID msg)
       P'.tellT "m" (m msg)
       P'.tellT "q" (q msg)
       P'.tellT "seed" (seed msg)
       P'.tellT "s" (s msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'challengeID, parse'instanceID, parse'm, parse'q, parse'seed, parse's]) P'.spaces
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
        parse'seed
         = P'.try
            (do
               v <- P'.getT "seed"
               Prelude'.return (\ o -> o{seed = v}))
        parse's
         = P'.try
            (do
               v <- P'.getT "s"
               Prelude'.return (\ o -> o{s = v}))