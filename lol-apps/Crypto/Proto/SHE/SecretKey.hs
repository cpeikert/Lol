{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.SHE.SecretKey (SecretKey(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.Lol.R as Lol (R)

data SecretKey = SecretKey{sk :: !(Lol.R), v :: !(P'.Double)}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable SecretKey where
  mergeAppend (SecretKey x'1 x'2) (SecretKey y'1 y'2) = SecretKey (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default SecretKey where
  defaultValue = SecretKey P'.defaultValue P'.defaultValue

instance P'.Wire SecretKey where
  wireSize ft' self'@(SecretKey x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 11 x'1 + P'.wireSizeReq 1 1 x'2)
  wirePut ft' self'@(SecretKey x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 11 x'1
             P'.wirePutReq 17 1 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{sk = P'.mergeAppend (sk old'Self) (new'Field)}) (P'.wireGet 11)
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{v = new'Field}) (P'.wireGet 1)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> SecretKey) SecretKey where
  getVal m' f' = f' m'

instance P'.GPB SecretKey

instance P'.ReflectDescriptor SecretKey where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 17]) (P'.fromDistinctAscList [10, 17])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".SHE.SecretKey\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"SHE\"], baseName = MName \"SecretKey\"}, descFilePath = [\"Crypto\",\"Proto\",\"SHE\",\"SecretKey.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".SHE.SecretKey.sk\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"SHE\",MName \"SecretKey\"], baseName' = FName \"sk\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lol.R\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"R\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".SHE.SecretKey.v\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"SHE\",MName \"SecretKey\"], baseName' = FName \"v\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType SecretKey where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg SecretKey where
  textPut msg
   = do
       P'.tellT "sk" (sk msg)
       P'.tellT "v" (v msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'sk, parse'v]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'sk
         = P'.try
            (do
               v <- P'.getT "sk"
               Prelude'.return (\ o -> o{sk = v}))
        parse'v
         = P'.try
            (do
               v <- P'.getT "v"
               Prelude'.return (\ o -> o{v = v}))