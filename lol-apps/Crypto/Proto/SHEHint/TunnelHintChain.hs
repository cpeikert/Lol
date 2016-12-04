{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.SHEHint.TunnelHintChain (TunnelHintChain(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.SHEHint.TunnelHints as SHEHint (TunnelHints)

data TunnelHintChain = TunnelHintChain{hints :: !(P'.Seq SHEHint.TunnelHints)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable TunnelHintChain where
  mergeAppend (TunnelHintChain x'1) (TunnelHintChain y'1) = TunnelHintChain (P'.mergeAppend x'1 y'1)

instance P'.Default TunnelHintChain where
  defaultValue = TunnelHintChain P'.defaultValue

instance P'.Wire TunnelHintChain where
  wireSize ft' self'@(TunnelHintChain x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(TunnelHintChain x'1)
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{hints = P'.append (hints old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TunnelHintChain) TunnelHintChain where
  getVal m' f' = f' m'

instance P'.GPB TunnelHintChain

instance P'.ReflectDescriptor TunnelHintChain where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".SHEHint.TunnelHintChain\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"SHEHint\"], baseName = MName \"TunnelHintChain\"}, descFilePath = [\"Crypto\",\"Proto\",\"SHEHint\",\"TunnelHintChain.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".SHEHint.TunnelHintChain.hints\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"SHEHint\",MName \"TunnelHintChain\"], baseName' = FName \"hints\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".SHEHint.TunnelHints\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"SHEHint\"], baseName = MName \"TunnelHints\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType TunnelHintChain where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TunnelHintChain where
  textPut msg
   = do
       P'.tellT "hints" (hints msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'hints]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'hints
         = P'.try
            (do
               v <- P'.getT "hints"
               Prelude'.return (\ o -> o{hints = P'.append (hints o) v}))