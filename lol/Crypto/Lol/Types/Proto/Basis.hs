{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Lol.Types.Proto.Basis (Basis(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Basis = POW
           | DEC
           | CRT
           deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable Basis

instance Prelude'.Bounded Basis where
  minBound = POW
  maxBound = CRT

instance P'.Default Basis where
  defaultValue = POW

toMaybe'Enum :: Prelude'.Int -> P'.Maybe Basis
toMaybe'Enum 0 = Prelude'.Just POW
toMaybe'Enum 1 = Prelude'.Just DEC
toMaybe'Enum 2 = Prelude'.Just CRT
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum Basis where
  fromEnum POW = 0
  fromEnum DEC = 1
  fromEnum CRT = 2
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Lol.CycMsg.Basis") . toMaybe'Enum
  succ POW = DEC
  succ DEC = CRT
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Lol.CycMsg.Basis"
  pred DEC = POW
  pred CRT = DEC
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Lol.CycMsg.Basis"

instance P'.Wire Basis where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB Basis

instance P'.MessageAPI msg' (msg' -> Basis) Basis where
  getVal m' f' = f' m'

instance P'.ReflectEnum Basis where
  reflectEnum = [(0, "POW", POW), (1, "DEC", DEC), (2, "CRT", CRT)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Lol.CycMsg.Basis") [] ["Lol", "CycMsg"] "Basis") ["Lol", "CycMsg", "Basis.hs"]
      [(0, "POW"), (1, "DEC"), (2, "CRT")]

instance P'.TextType Basis where
  tellT = P'.tellShow
  getT = P'.getRead