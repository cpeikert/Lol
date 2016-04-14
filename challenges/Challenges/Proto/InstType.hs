{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Challenges.Proto.InstType (InstType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data InstType = ContLWE
              | DiscLWE
              | LWR
              deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable InstType

instance Prelude'.Bounded InstType where
  minBound = ContLWE
  maxBound = LWR

instance P'.Default InstType where
  defaultValue = ContLWE

toMaybe'Enum :: Prelude'.Int -> P'.Maybe InstType
toMaybe'Enum 0 = Prelude'.Just ContLWE
toMaybe'Enum 1 = Prelude'.Just DiscLWE
toMaybe'Enum 2 = Prelude'.Just LWR
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum InstType where
  fromEnum ContLWE = 0
  fromEnum DiscLWE = 1
  fromEnum LWR = 2
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Lwe.LWEInstance.InstType") . toMaybe'Enum
  succ ContLWE = DiscLWE
  succ DiscLWE = LWR
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Lwe.LWEInstance.InstType"
  pred DiscLWE = ContLWE
  pred LWR = DiscLWE
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Lwe.LWEInstance.InstType"

instance P'.Wire InstType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB InstType

instance P'.MessageAPI msg' (msg' -> InstType) InstType where
  getVal m' f' = f' m'

instance P'.ReflectEnum InstType where
  reflectEnum = [(0, "ContLWE", ContLWE), (1, "DiscLWE", DiscLWE), (2, "LWR", LWR)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Lwe.LWEInstance.InstType") [] ["Lwe", "LWEInstance"] "InstType")
      ["Lwe", "LWEInstance", "InstType.hs"]
      [(0, "ContLWE"), (1, "DiscLWE"), (2, "LWR")]

instance P'.TextType InstType where
  tellT = P'.tellShow
  getT = P'.getRead