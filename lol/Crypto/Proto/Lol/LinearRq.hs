{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.Lol.LinearRq (LinearRq(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.Lol.RqProduct as Lol (RqProduct)

data LinearRq = LinearRq{e :: !(P'.Word32), r :: !(P'.Word32), coeffs :: !(P'.Seq Lol.RqProduct)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable LinearRq where
  mergeAppend (LinearRq x'1 x'2 x'3) (LinearRq y'1 y'2 y'3)
   = LinearRq (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)

instance P'.Default LinearRq where
  defaultValue = LinearRq P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire LinearRq where
  wireSize ft' self'@(LinearRq x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 13 x'1 + P'.wireSizeReq 1 13 x'2 + P'.wireSizeRep 1 11 x'3)
  wirePut ft' self'@(LinearRq x'1 x'2 x'3)
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
             P'.wirePutReq 16 13 x'2
             P'.wirePutRep 26 11 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{e = new'Field}) (P'.wireGet 13)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{r = new'Field}) (P'.wireGet 13)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{coeffs = P'.append (coeffs old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> LinearRq) LinearRq where
  getVal m' f' = f' m'

instance P'.GPB LinearRq

instance P'.ReflectDescriptor LinearRq where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16]) (P'.fromDistinctAscList [8, 16, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Lol.LinearRq\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"LinearRq\"}, descFilePath = [\"Crypto\",\"Proto\",\"Lol\",\"LinearRq.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.LinearRq.e\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"Lol\",MName \"LinearRq\"], baseName' = FName \"e\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.LinearRq.r\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"Lol\",MName \"LinearRq\"], baseName' = FName \"r\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.LinearRq.coeffs\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"Lol\",MName \"LinearRq\"], baseName' = FName \"coeffs\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lol.RqProduct\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"RqProduct\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType LinearRq where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg LinearRq where
  textPut msg
   = do
       P'.tellT "e" (e msg)
       P'.tellT "r" (r msg)
       P'.tellT "coeffs" (coeffs msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'e, parse'r, parse'coeffs]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'e
         = P'.try
            (do
               v <- P'.getT "e"
               Prelude'.return (\ o -> o{e = v}))
        parse'r
         = P'.try
            (do
               v <- P'.getT "r"
               Prelude'.return (\ o -> o{r = v}))
        parse'coeffs
         = P'.try
            (do
               v <- P'.getT "coeffs"
               Prelude'.return (\ o -> o{coeffs = P'.append (coeffs o) v}))