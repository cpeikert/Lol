{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.Lol.TypeRep (TypeRep(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data TypeRep = TypeRep{a :: !(P'.Word64), b :: !(P'.Word64)}
             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable TypeRep where
  mergeAppend (TypeRep x'1 x'2) (TypeRep y'1 y'2) = TypeRep (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default TypeRep where
  defaultValue = TypeRep P'.defaultValue P'.defaultValue

instance P'.Wire TypeRep where
  wireSize ft' self'@(TypeRep x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 4 x'1 + P'.wireSizeReq 1 4 x'2)
  wirePut ft' self'@(TypeRep x'1 x'2)
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
             P'.wirePutReq 16 4 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{a = new'Field}) (P'.wireGet 4)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{b = new'Field}) (P'.wireGet 4)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TypeRep) TypeRep where
  getVal m' f' = f' m'

instance P'.GPB TypeRep

instance P'.ReflectDescriptor TypeRep where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16]) (P'.fromDistinctAscList [8, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Lol.TypeRep\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"TypeRep\"}, descFilePath = [\"Crypto\",\"Proto\",\"Lol\",\"TypeRep.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.TypeRep.a\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"Lol\",MName \"TypeRep\"], baseName' = FName \"a\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.TypeRep.b\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"Lol\",MName \"TypeRep\"], baseName' = FName \"b\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType TypeRep where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TypeRep where
  textPut msg
   = do
       P'.tellT "a" (a msg)
       P'.tellT "b" (b msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'a, parse'b]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'a
         = P'.try
            (do
               v <- P'.getT "a"
               Prelude'.return (\ o -> o{a = v}))
        parse'b
         = P'.try
            (do
               v <- P'.getT "b"
               Prelude'.return (\ o -> o{b = v}))