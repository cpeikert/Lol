{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.HomomPRF.LinearFuncChain (LinearFuncChain(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.Lol.LinearRq as Lol (LinearRq)

data LinearFuncChain = LinearFuncChain{funcs :: !(P'.Seq Lol.LinearRq)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable LinearFuncChain where
  mergeAppend (LinearFuncChain x'1) (LinearFuncChain y'1) = LinearFuncChain (P'.mergeAppend x'1 y'1)

instance P'.Default LinearFuncChain where
  defaultValue = LinearFuncChain P'.defaultValue

instance P'.Wire LinearFuncChain where
  wireSize ft' self'@(LinearFuncChain x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(LinearFuncChain x'1)
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{funcs = P'.append (funcs old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> LinearFuncChain) LinearFuncChain where
  getVal m' f' = f' m'

instance P'.GPB LinearFuncChain

instance P'.ReflectDescriptor LinearFuncChain where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".HomomPRF.LinearFuncChain\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"HomomPRF\"], baseName = MName \"LinearFuncChain\"}, descFilePath = [\"Crypto\",\"Proto\",\"HomomPRF\",\"LinearFuncChain.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".HomomPRF.LinearFuncChain.funcs\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"HomomPRF\",MName \"LinearFuncChain\"], baseName' = FName \"funcs\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lol.LinearRq\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"LinearRq\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType LinearFuncChain where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg LinearFuncChain where
  textPut msg
   = do
       P'.tellT "funcs" (funcs msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'funcs]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'funcs
         = P'.try
            (do
               v <- P'.getT "funcs"
               Prelude'.return (\ o -> o{funcs = P'.append (funcs o) v}))