{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.RLWE.Challenges.ChallengeType (ChallengeType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data ChallengeType = Cont
                   | Disc
                   | RLWR
                   deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data,
                             Prelude'.Generic)

instance P'.Mergeable ChallengeType

instance Prelude'.Bounded ChallengeType where
  minBound = Cont
  maxBound = RLWR

instance P'.Default ChallengeType where
  defaultValue = Cont

toMaybe'Enum :: Prelude'.Int -> P'.Maybe ChallengeType
toMaybe'Enum 0 = Prelude'.Just Cont
toMaybe'Enum 1 = Prelude'.Just Disc
toMaybe'Enum 2 = Prelude'.Just RLWR
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum ChallengeType where
  fromEnum Cont = 0
  fromEnum Disc = 1
  fromEnum RLWR = 2
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Crypto.Proto.RLWE.Challenges.ChallengeType") .
      toMaybe'Enum
  succ Cont = Disc
  succ Disc = RLWR
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Crypto.Proto.RLWE.Challenges.ChallengeType"
  pred Disc = Cont
  pred RLWR = Disc
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Crypto.Proto.RLWE.Challenges.ChallengeType"

instance P'.Wire ChallengeType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB ChallengeType

instance P'.MessageAPI msg' (msg' -> ChallengeType) ChallengeType where
  getVal m' f' = f' m'

instance P'.ReflectEnum ChallengeType where
  reflectEnum = [(0, "Cont", Cont), (1, "Disc", Disc), (2, "RLWR", RLWR)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Challenges.ChallengeType") ["Crypto", "Proto", "RLWE"] ["Challenges"] "ChallengeType")
      ["Crypto", "Proto", "RLWE", "Challenges", "ChallengeType.hs"]
      [(0, "Cont"), (1, "Disc"), (2, "RLWR")]

instance P'.TextType ChallengeType where
  tellT = P'.tellShow
  getT = P'.getRead