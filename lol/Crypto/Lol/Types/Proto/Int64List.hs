{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Lol.Types.Proto.Int64List (Int64List(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Int64List = Int64List{q :: !(P'.Maybe P'.Word64), xs :: !(P'.Seq P'.Int64)}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable Int64List where
  mergeAppend (Int64List x'1 x'2) (Int64List y'1 y'2) = Int64List (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default Int64List where
  defaultValue = Int64List P'.defaultValue P'.defaultValue

instance P'.Wire Int64List where
  wireSize ft' self'@(Int64List x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 4 x'1 + P'.wireSizeRep 1 18 x'2)
  wirePut ft' self'@(Int64List x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 4 x'1
             P'.wirePutRep 16 18 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{q = Prelude'.Just new'Field}) (P'.wireGet 4)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{xs = P'.append (xs old'Self) new'Field}) (P'.wireGet 18)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{xs = P'.mergeAppend (xs old'Self) new'Field}) (P'.wireGetPacked 18)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Int64List) Int64List where
  getVal m' f' = f' m'

instance P'.GPB Int64List

instance P'.ReflectDescriptor Int64List where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Lol.Int64List\", haskellPrefix = [], parentModule = [MName \"Lol\"], baseName = MName \"Int64List\"}, descFilePath = [\"Lol\",\"Int64List.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.Int64List.q\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"Int64List\"], baseName' = FName \"q\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Lol.Int64List.xs\", haskellPrefix' = [], parentModule' = [MName \"Lol\",MName \"Int64List\"], baseName' = FName \"xs\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Just (WireTag {getWireTag = 16},WireTag {getWireTag = 18}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Int64List where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Int64List where
  textPut msg
   = do
       P'.tellT "q" (q msg)
       P'.tellT "xs" (xs msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'q, parse'xs]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'q
         = P'.try
            (do
               v <- P'.getT "q"
               Prelude'.return (\ o -> o{q = v}))
        parse'xs
         = P'.try
            (do
               v <- P'.getT "xs"
               Prelude'.return (\ o -> o{xs = P'.append (xs o) v}))