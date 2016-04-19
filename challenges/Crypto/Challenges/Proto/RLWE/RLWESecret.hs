{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Challenges.Proto.RLWE.RLWESecret (RLWESecret(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Lol.Types.Proto.R as Lol (R)

data RLWESecret = RLWESecret{id :: !(P'.Word32), m :: !(P'.Word64), secret :: !(Lol.R)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable RLWESecret where
  mergeAppend (RLWESecret x'1 x'2 x'3) (RLWESecret y'1 y'2 y'3)
   = RLWESecret (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)

instance P'.Default RLWESecret where
  defaultValue = RLWESecret P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire RLWESecret where
  wireSize ft' self'@(RLWESecret x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 13 x'1 + P'.wireSizeReq 1 4 x'2 + P'.wireSizeReq 1 11 x'3)
  wirePut ft' self'@(RLWESecret x'1 x'2 x'3)
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
             P'.wirePutReq 16 4 x'2
             P'.wirePutReq 26 11 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{id = new'Field}) (P'.wireGet 13)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{m = new'Field}) (P'.wireGet 4)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{secret = P'.mergeAppend (secret old'Self) (new'Field)}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> RLWESecret) RLWESecret where
  getVal m' f' = f' m'

instance P'.GPB RLWESecret

instance P'.ReflectDescriptor RLWESecret where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 26]) (P'.fromDistinctAscList [8, 16, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RLWE.RLWESecret\", haskellPrefix = [MName \"Challenges\",MName \"Proto\"], parentModule = [MName \"RLWE\"], baseName = MName \"RLWESecret\"}, descFilePath = [\"Challenges\",\"Proto\",\"RLWE\",\"RLWESecret.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.RLWESecret.id\", haskellPrefix' = [MName \"Challenges\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"RLWESecret\"], baseName' = FName \"id\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.RLWESecret.m\", haskellPrefix' = [MName \"Challenges\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"RLWESecret\"], baseName' = FName \"m\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RLWE.RLWESecret.secret\", haskellPrefix' = [MName \"Challenges\",MName \"Proto\"], parentModule' = [MName \"RLWE\",MName \"RLWESecret\"], baseName' = FName \"secret\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Lol.R\", haskellPrefix = [MName \"Challenges\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"R\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType RLWESecret where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg RLWESecret where
  textPut msg
   = do
       P'.tellT "id" (id msg)
       P'.tellT "m" (m msg)
       P'.tellT "secret" (secret msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'id, parse'm, parse'secret]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'id
         = P'.try
            (do
               v <- P'.getT "id"
               Prelude'.return (\ o -> o{id = v}))
        parse'm
         = P'.try
            (do
               v <- P'.getT "m"
               Prelude'.return (\ o -> o{m = v}))
        parse'secret
         = P'.try
            (do
               v <- P'.getT "secret"
               Prelude'.return (\ o -> o{secret = v}))