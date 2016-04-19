{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Challenges.Proto.RLWE.RLWEInstanceDisc (RLWEInstanceDisc(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Challenges.Proto.RLWE.RLWESampleDisc as RLWE (RLWESampleDisc)

data RLWEInstanceDisc = RLWEInstanceDisc{id :: !(P'.Word32), m :: !(P'.Word64), q :: !(P'.Word64), svar :: !(P'.Double),
                                         bound :: !(P'.Word64), samples :: !(P'.Seq RLWE.RLWESampleDisc)}
                      deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable RLWEInstanceDisc where
  mergeAppend (RLWEInstanceDisc x'1 x'2 x'3 x'4 x'5 x'6) (RLWEInstanceDisc y'1 y'2 y'3 y'4 y'5 y'6)
   = RLWEInstanceDisc (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)

instance P'.Default RLWEInstanceDisc where
  defaultValue = RLWEInstanceDisc P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire RLWEInstanceDisc where
  wireSize ft' self'@(RLWEInstanceDisc x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 13 x'1 + P'.wireSizeReq 1 4 x'2 + P'.wireSizeReq 1 4 x'3 + P'.wireSizeReq 1 1 x'4 +
             P'.wireSizeReq 1 4 x'5
             + P'.wireSizeRep 1 11 x'6)
  wirePut ft' self'@(RLWEInstanceDisc x'1 x'2 x'3 x'4 x'5 x'6)
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
             P'.wirePutReq 40 4 x'5
             P'.wirePutRep 50 11 x'6
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
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{bound = new'Field}) (P'.wireGet 4)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{samples = P'.append (samples old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> RLWEInstanceDisc) RLWEInstanceDisc where
  getVal m' f' = f' m'

instance P'.GPB RLWEInstanceDisc

instance P'.ReflectDescriptor RLWEInstanceDisc where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 24, 33, 40]) (P'.fromDistinctAscList [8, 16, 24, 33, 40, 50])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RLWE.RLWEInstanceDisc\", haskellPrefix = [MName \"Challenges\",MName \"Proto\"], parentModule = [MName \"RLWE\"], baseName = MName \"RLWEInstanceDisc\"}, descFilePath = [\"Challenges\",\"Proto\",\"RLWE\",\"RLWEInstanceDisc.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.RLWEInstanceDisc.id\", haskellPrefix' = [MName \"Challenges\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"RLWEInstanceDisc\"], baseName' = FName \"id\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.RLWEInstanceDisc.m\", haskellPrefix' = [MName \"Challenges\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"RLWEInstanceDisc\"], baseName' = FName \"m\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.RLWEInstanceDisc.q\", haskellPrefix' = [MName \"Challenges\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"RLWEInstanceDisc\"], baseName' = FName \"q\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.RLWEInstanceDisc.svar\", haskellPrefix' = [MName \"Challenges\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"RLWEInstanceDisc\"], baseName' = FName \"svar\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 33}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.RLWEInstanceDisc.bound\", haskellPrefix' = [MName \"Challenges\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"RLWEInstanceDisc\"], baseName' = FName \"bound\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.RLWEInstanceDisc.samples\", haskellPrefix' = [MName \"Challenges\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"RLWEInstanceDisc\"], baseName' = FName \"samples\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RLWE.RLWESampleDisc\", haskellPrefix = [MName \"Challenges\",MName \"Proto\"], parentModule = [MName \"RLWE\"], baseName = MName \"RLWESampleDisc\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType RLWEInstanceDisc where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg RLWEInstanceDisc where
  textPut msg
   = do
       P'.tellT "id" (id msg)
       P'.tellT "m" (m msg)
       P'.tellT "q" (q msg)
       P'.tellT "svar" (svar msg)
       P'.tellT "bound" (bound msg)
       P'.tellT "samples" (samples msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'id, parse'm, parse'q, parse'svar, parse'bound, parse'samples]) P'.spaces
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
        parse'bound
         = P'.try
            (do
               v <- P'.getT "bound"
               Prelude'.return (\ o -> o{bound = v}))
        parse'samples
         = P'.try
            (do
               v <- P'.getT "samples"
               Prelude'.return (\ o -> o{samples = P'.append (samples o) v}))