{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.Lol.R (R(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data R = R{m :: !(P'.Word32), xs :: !(P'.Seq P'.Int64)}
       deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable R where
  mergeAppend (R x'1 x'2) (R y'1 y'2) = R (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default R where
  defaultValue = R P'.defaultValue P'.defaultValue

instance P'.Wire R where
  wireSize ft' self'@(R x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 13 x'1 + P'.wireSizeRep 1 18 x'2)
  wirePut ft' self'@(R x'1 x'2)
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
             P'.wirePutRep 16 18 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{m = new'Field}) (P'.wireGet 13)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{xs = P'.append (xs old'Self) new'Field}) (P'.wireGet 18)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{xs = P'.mergeAppend (xs old'Self) new'Field}) (P'.wireGetPacked 18)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> R) R where
  getVal m' f' = f' m'

instance P'.GPB R

instance P'.ReflectDescriptor R where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8]) (P'.fromDistinctAscList [8, 16, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Lol.R\", haskellPrefix = [MName \"Crypto\",MName \"Proto\"], parentModule = [MName \"Lol\"], baseName = MName \"R\"}, descFilePath = [\"Crypto\",\"Proto\",\"Lol\",\"R.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.R.m\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"Lol\",MName \"R\"], baseName' = FName \"m\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.R.xs\", haskellPrefix' = [MName \"Crypto\",MName \"Proto\"], parentModule' = [MName \"Lol\",MName \"R\"], baseName' = FName \"xs\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Just (WireTag {getWireTag = 16},WireTag {getWireTag = 18}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType R where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg R where
  textPut msg
   = do
       P'.tellT "m" (m msg)
       P'.tellT "xs" (xs msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'm, parse'xs]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'm
         = P'.try
            (do
               v <- P'.getT "m"
               Prelude'.return (\ o -> o{m = v}))
        parse'xs
         = P'.try
            (do
               v <- P'.getT "xs"
               Prelude'.return (\ o -> o{xs = P'.append (xs o) v}))