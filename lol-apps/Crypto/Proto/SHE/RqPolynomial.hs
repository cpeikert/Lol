{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.SHE.RqPolynomial (RqPolynomial(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.Lol.RqProduct as Lol (RqProduct)

data RqPolynomial = RqPolynomial{coeffs :: !(P'.Seq Lol.RqProduct)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable RqPolynomial where
  mergeAppend (RqPolynomial x'1) (RqPolynomial y'1) = RqPolynomial (P'.mergeAppend x'1 y'1)

instance P'.Default RqPolynomial where
  defaultValue = RqPolynomial P'.defaultValue

instance P'.Wire RqPolynomial where
  wireSize ft' self'@(RqPolynomial x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(RqPolynomial x'1)
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{coeffs = P'.append (coeffs old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> RqPolynomial) RqPolynomial where
  getVal m' f' = f' m'

instance P'.GPB RqPolynomial

instance P'.ReflectDescriptor RqPolynomial where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".SHE.RqPolynomial\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"SHE\"], baseName = MName \"RqPolynomial\"}, descFilePath = [\"Crypto\",\"Proto\",\"SHE\",\"RqPolynomial.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".SHE.RqPolynomial.coeffs\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"SHE\",MName \"RqPolynomial\"], baseName' = FName \"coeffs\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lol.RqProduct\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"RqProduct\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType RqPolynomial where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg RqPolynomial where
  textPut msg
   = do
       P'.tellT "coeffs" (coeffs msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'coeffs]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'coeffs
         = P'.try
            (do
               v <- P'.getT "coeffs"
               Prelude'.return (\ o -> o{coeffs = P'.append (coeffs o) v}))