{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Challenges.Proto.LWEInstance (LWEInstance(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Challenges.Proto.LWESample as Lol (LWESample)

data LWEInstance = LWEInstance{id :: !(P'.Word32), m :: !(P'.Word64), q :: !(P'.Word64), svar :: !(P'.Double),
                               samples :: !(P'.Seq Lol.LWESample)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable LWEInstance where
  mergeAppend (LWEInstance x'1 x'2 x'3 x'4 x'5) (LWEInstance y'1 y'2 y'3 y'4 y'5)
   = LWEInstance (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)

instance P'.Default LWEInstance where
  defaultValue = LWEInstance P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire LWEInstance where
  wireSize ft' self'@(LWEInstance x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 13 x'1 + P'.wireSizeReq 1 4 x'2 + P'.wireSizeReq 1 4 x'3 + P'.wireSizeReq 1 1 x'4 +
             P'.wireSizeRep 1 11 x'5)
  wirePut ft' self'@(LWEInstance x'1 x'2 x'3 x'4 x'5)
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
             P'.wirePutReq 16 4 x'2
             P'.wirePutReq 24 4 x'3
             P'.wirePutReq 33 1 x'4
             P'.wirePutRep 42 11 x'5
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{id = new'Field}) (P'.wireGet 13)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{m = new'Field}) (P'.wireGet 4)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{q = new'Field}) (P'.wireGet 4)
             33 -> Prelude'.fmap (\ !new'Field -> old'Self{svar = new'Field}) (P'.wireGet 1)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{samples = P'.append (samples old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> LWEInstance) LWEInstance where
  getVal m' f' = f' m'

instance P'.GPB LWEInstance

instance P'.ReflectDescriptor LWEInstance where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 24, 33]) (P'.fromDistinctAscList [8, 16, 24, 33, 42])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Lol.LWEInstance\", haskellPrefix = [], parentModule = [MName \"Lol\"], baseName = MName \"LWEInstance\"}, descFilePath = [\"Lol\",\"LWEInstance.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.LWEInstance.id\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"LWEInstance\"], baseName' = FName \"id\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.LWEInstance.m\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"LWEInstance\"], baseName' = FName \"m\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.LWEInstance.q\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"LWEInstance\"], baseName' = FName \"q\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.LWEInstance.svar\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"LWEInstance\"], baseName' = FName \"svar\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 33}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.LWEInstance.samples\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"LWEInstance\"], baseName' = FName \"samples\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lol.LWESample\", haskellPrefix = [], parentModule = [MName \"Lol\"], baseName = MName \"LWESample\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType LWEInstance where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg LWEInstance where
  textPut msg
   = do
       P'.tellT "id" (id msg)
       P'.tellT "m" (m msg)
       P'.tellT "q" (q msg)
       P'.tellT "svar" (svar msg)
       P'.tellT "samples" (samples msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'id, parse'm, parse'q, parse'svar, parse'samples]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'id
         = P'.try
            (do
               v <- P'.getT "id"
               Prelude'.return (\ o -> o{id = v}))
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
        parse'svar
         = P'.try
            (do
               v <- P'.getT "svar"
               Prelude'.return (\ o -> o{svar = v}))
        parse'samples
         = P'.try
            (do
               v <- P'.getT "samples"
               Prelude'.return (\ o -> o{samples = P'.append (samples o) v}))