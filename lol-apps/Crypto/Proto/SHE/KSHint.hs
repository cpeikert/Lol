{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.SHE.KSHint (KSHint(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.Lol.TypeRep as Lol (TypeRep)
import qualified Crypto.Proto.SHE.RqPolynomial as SHE (RqPolynomial)

data KSHint = KSHint{hint :: !(P'.Seq SHE.RqPolynomial), gad :: !(Lol.TypeRep)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable KSHint where
  mergeAppend (KSHint x'1 x'2) (KSHint y'1 y'2) = KSHint (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default KSHint where
  defaultValue = KSHint P'.defaultValue P'.defaultValue

instance P'.Wire KSHint where
  wireSize ft' self'@(KSHint x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeReq 1 11 x'2)
  wirePut ft' self'@(KSHint x'1 x'2)
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
             P'.wirePutReq 18 11 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{hint = P'.append (hint old'Self) new'Field}) (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{gad = P'.mergeAppend (gad old'Self) (new'Field)}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> KSHint) KSHint where
  getVal m' f' = f' m'

instance P'.GPB KSHint

instance P'.ReflectDescriptor KSHint where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [18]) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".SHE.KSHint\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"SHE\"], baseName = MName \"KSHint\"}, descFilePath = [\"Crypto\",\"Proto\",\"SHE\",\"KSHint.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".SHE.KSHint.hint\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"SHE\",MName \"KSHint\"], baseName' = FName \"hint\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".SHE.RqPolynomial\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"SHE\"], baseName = MName \"RqPolynomial\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".SHE.KSHint.gad\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"SHE\",MName \"KSHint\"], baseName' = FName \"gad\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lol.TypeRep\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"TypeRep\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType KSHint where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg KSHint where
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