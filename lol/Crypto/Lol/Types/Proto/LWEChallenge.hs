{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Lol.Types.Proto.LWEChallenge (LWEChallenge(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Lol.Types.Proto.LWEInstance as Proto (LWEInstance)

data LWEChallenge = LWEChallenge{beaconTime :: !(P'.Word64), bitOffset :: !(P'.Word32), svar :: !(P'.Double),
                                 instances :: !(P'.Seq Proto.LWEInstance)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable LWEChallenge where
  mergeAppend (LWEChallenge x'1 x'2 x'3 x'4) (LWEChallenge y'1 y'2 y'3 y'4)
   = LWEChallenge (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default LWEChallenge where
  defaultValue = LWEChallenge P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire LWEChallenge where
  wireSize ft' self'@(LWEChallenge x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 4 x'1 + P'.wireSizeReq 1 13 x'2 + P'.wireSizeReq 1 1 x'3 + P'.wireSizeRep 1 11 x'4)
  wirePut ft' self'@(LWEChallenge x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 4 x'1
             P'.wirePutReq 16 13 x'2
             P'.wirePutReq 25 1 x'3
             P'.wirePutRep 34 11 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{beaconTime = new'Field}) (P'.wireGet 4)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{bitOffset = new'Field}) (P'.wireGet 13)
             25 -> Prelude'.fmap (\ !new'Field -> old'Self{svar = new'Field}) (P'.wireGet 1)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{instances = P'.append (instances old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> LWEChallenge) LWEChallenge where
  getVal m' f' = f' m'

instance P'.GPB LWEChallenge

instance P'.ReflectDescriptor LWEChallenge where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 25]) (P'.fromDistinctAscList [8, 16, 25, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Proto.LWEChallenge\", haskellPrefix = [], parentModule = [MName \"Proto\"], baseName = MName \"LWEChallenge\"}, descFilePath = [\"Proto\",\"LWEChallenge.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.LWEChallenge.beaconTime\", haskellPrefix' = [], parentModule' = [MName \"Proto\",MName \"LWEChallenge\"], baseName' = FName \"beaconTime\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.LWEChallenge.bitOffset\", haskellPrefix' = [], parentModule' = [MName \"Proto\",MName \"LWEChallenge\"], baseName' = FName \"bitOffset\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.LWEChallenge.svar\", haskellPrefix' = [], parentModule' = [MName \"Proto\",MName \"LWEChallenge\"], baseName' = FName \"svar\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 25}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.LWEChallenge.instances\", haskellPrefix' = [], parentModule' = [MName \"Proto\",MName \"LWEChallenge\"], baseName' = FName \"instances\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Proto.LWEInstance\", haskellPrefix = [], parentModule = [MName \"Proto\"], baseName = MName \"LWEInstance\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType LWEChallenge where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg LWEChallenge where
  textPut msg
   = do
       P'.tellT "beaconTime" (beaconTime msg)
       P'.tellT "bitOffset" (bitOffset msg)
       P'.tellT "svar" (svar msg)
       P'.tellT "instances" (instances msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'beaconTime, parse'bitOffset, parse'svar, parse'instances]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'beaconTime
         = P'.try
            (do
               v <- P'.getT "beaconTime"
               Prelude'.return (\ o -> o{beaconTime = v}))
        parse'bitOffset
         = P'.try
            (do
               v <- P'.getT "bitOffset"
               Prelude'.return (\ o -> o{bitOffset = v}))
        parse'svar
         = P'.try
            (do
               v <- P'.getT "svar"
               Prelude'.return (\ o -> o{svar = v}))
        parse'instances
         = P'.try
            (do
               v <- P'.getT "instances"
               Prelude'.return (\ o -> o{instances = P'.append (instances o) v}))