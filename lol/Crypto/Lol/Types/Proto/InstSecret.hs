{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Lol.Types.Proto.InstSecret (InstSecret(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Lol.Types.Proto.CycMsg as Proto (CycMsg)

data InstSecret = InstSecret{id :: !(P'.Word32), secret :: !(Proto.CycMsg)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable InstSecret where
  mergeAppend (InstSecret x'1 x'2) (InstSecret y'1 y'2) = InstSecret (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default InstSecret where
  defaultValue = InstSecret P'.defaultValue P'.defaultValue

instance P'.Wire InstSecret where
  wireSize ft' self'@(InstSecret x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 13 x'1 + P'.wireSizeReq 1 11 x'2)
  wirePut ft' self'@(InstSecret x'1 x'2)
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
             P'.wirePutReq 18 11 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{id = new'Field}) (P'.wireGet 13)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{secret = P'.mergeAppend (secret old'Self) (new'Field)}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> InstSecret) InstSecret where
  getVal m' f' = f' m'

instance P'.GPB InstSecret

instance P'.ReflectDescriptor InstSecret where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 18]) (P'.fromDistinctAscList [8, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Proto.InstSecret\", haskellPrefix = [], parentModule = [MName \"Proto\"], baseName = MName \"InstSecret\"}, descFilePath = [\"Proto\",\"InstSecret.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.InstSecret.id\", haskellPrefix' = [], parentModule' = [MName \"Proto\",MName \"InstSecret\"], baseName' = FName \"id\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.InstSecret.secret\", haskellPrefix' = [], parentModule' = [MName \"Proto\",MName \"InstSecret\"], baseName' = FName \"secret\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Proto.CycMsg\", haskellPrefix = [], parentModule = [MName \"Proto\"], baseName = MName \"CycMsg\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType InstSecret where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg InstSecret where
  textPut msg
   = do
       P'.tellT "id" (id msg)
       P'.tellT "secret" (secret msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'id, parse'secret]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'id
         = P'.try
            (do
               v <- P'.getT "id"
               Prelude'.return (\ o -> o{id = v}))
        parse'secret
         = P'.try
            (do
               v <- P'.getT "secret"
               Prelude'.return (\ o -> o{secret = v}))