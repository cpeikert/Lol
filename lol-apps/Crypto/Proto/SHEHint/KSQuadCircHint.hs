{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.SHEHint.KSQuadCircHint (KSQuadCircHint(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.SHEHint.RqPolynomial as SHEHint (RqPolynomial)

data KSQuadCircHint = KSQuadCircHint{hint :: !(P'.Seq SHEHint.RqPolynomial), gad :: !(P'.Utf8)}
                    deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable KSQuadCircHint where
  mergeAppend (KSQuadCircHint x'1 x'2) (KSQuadCircHint y'1 y'2) = KSQuadCircHint (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default KSQuadCircHint where
  defaultValue = KSQuadCircHint P'.defaultValue P'.defaultValue

instance P'.Wire KSQuadCircHint where
  wireSize ft' self'@(KSQuadCircHint x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeReq 1 9 x'2)
  wirePut ft' self'@(KSQuadCircHint x'1 x'2)
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
             P'.wirePutReq 18 9 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{hint = P'.append (hint old'Self) new'Field}) (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{gad = new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> KSQuadCircHint) KSQuadCircHint where
  getVal m' f' = f' m'

instance P'.GPB KSQuadCircHint

instance P'.ReflectDescriptor KSQuadCircHint where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [18]) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".SHEHint.KSQuadCircHint\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"SHEHint\"], baseName = MName \"KSQuadCircHint\"}, descFilePath = [\"Crypto\",\"Proto\",\"SHEHint\",\"KSQuadCircHint.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".SHEHint.KSQuadCircHint.hint\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"SHEHint\",MName \"KSQuadCircHint\"], baseName' = FName \"hint\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".SHEHint.RqPolynomial\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"SHEHint\"], baseName = MName \"RqPolynomial\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".SHEHint.KSQuadCircHint.gad\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"SHEHint\",MName \"KSQuadCircHint\"], baseName' = FName \"gad\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType KSQuadCircHint where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg KSQuadCircHint where
  textPut msg
   = do
       P'.tellT "hint" (hint msg)
       P'.tellT "gad" (gad msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'hint, parse'gad]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'hint
         = P'.try
            (do
               v <- P'.getT "hint"
               Prelude'.return (\ o -> o{hint = P'.append (hint o) v}))
        parse'gad
         = P'.try
            (do
               v <- P'.getT "gad"
               Prelude'.return (\ o -> o{gad = v}))