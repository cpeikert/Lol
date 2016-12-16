{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.HomomPRF.TunnelInfoChain (TunnelInfoChain(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.SHE.TunnelInfo as SHE (TunnelInfo)

data TunnelInfoChain = TunnelInfoChain{hints :: !(P'.Seq SHE.TunnelInfo)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable TunnelInfoChain where
  mergeAppend (TunnelInfoChain x'1) (TunnelInfoChain y'1) = TunnelInfoChain (P'.mergeAppend x'1 y'1)

instance P'.Default TunnelInfoChain where
  defaultValue = TunnelInfoChain P'.defaultValue

instance P'.Wire TunnelInfoChain where
  wireSize ft' self'@(TunnelInfoChain x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(TunnelInfoChain x'1)
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

instance P'.MessageAPI msg' (msg' -> TunnelInfoChain) TunnelInfoChain where
  getVal m' f' = f' m'

instance P'.GPB TunnelInfoChain

instance P'.ReflectDescriptor TunnelInfoChain where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".HomomPRF.TunnelInfoChain\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"HomomPRF\"], baseName = MName \"TunnelInfoChain\"}, descFilePath = [\"Crypto\",\"Proto\",\"HomomPRF\",\"TunnelInfoChain.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".HomomPRF.TunnelInfoChain.hints\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"HomomPRF\",MName \"TunnelInfoChain\"], baseName' = FName \"hints\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".SHE.TunnelInfo\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"SHE\"], baseName = MName \"TunnelInfo\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType TunnelInfoChain where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TunnelInfoChain where
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