{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.Lol.KqProduct (KqProduct(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.Lol.Kq1 as Lol (Kq1)

data KqProduct = KqProduct{kqlist :: !(P'.Seq Lol.Kq1)}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable KqProduct where
  mergeAppend (KqProduct x'1) (KqProduct y'1) = KqProduct (P'.mergeAppend x'1 y'1)

instance P'.Default KqProduct where
  defaultValue = KqProduct P'.defaultValue

instance P'.Wire KqProduct where
  wireSize ft' self'@(KqProduct x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(KqProduct x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{kqlist = P'.append (kqlist old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> KqProduct) KqProduct where
  getVal m' f' = f' m'

instance P'.GPB KqProduct

instance P'.ReflectDescriptor KqProduct where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Lol.KqProduct\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"KqProduct\"}, descFilePath = [\"Crypto\",\"Proto\",\"Lol\",\"KqProduct.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.KqProduct.kqlist\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"Lol\",MName \"KqProduct\"], baseName' = FName \"kqlist\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lol.Kq1\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"Kq1\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType KqProduct where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg KqProduct where
  textPut msg
   = do
       P'.tellT "kqlist" (kqlist msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'kqlist]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'kqlist
         = P'.try
            (do
               v <- P'.getT "kqlist"
               Prelude'.return (\ o -> o{kqlist = P'.append (kqlist o) v}))