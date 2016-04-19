{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Challenges.RLWE.Proto.RLWE.Instance.InstType where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Challenges.RLWE.Proto.RLWE.InstanceCont as RLWE (InstanceCont)
import qualified Crypto.Challenges.RLWE.Proto.RLWE.InstanceDisc as RLWE (InstanceDisc)
import qualified Crypto.Challenges.RLWE.Proto.RLWE.InstanceRound as RLWE (InstanceRound)

data InstType = InstCont{instCont :: (RLWE.InstanceCont)}
              | InstDisc{instDisc :: (RLWE.InstanceDisc)}
              | InstRound{instRound :: (RLWE.InstanceRound)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)
get'instCont x
 = case x of
     InstCont instCont -> Prelude'.Just instCont
     _ -> Prelude'.Nothing
get'instDisc x
 = case x of
     InstDisc instDisc -> Prelude'.Just instDisc
     _ -> Prelude'.Nothing
get'instRound x
 = case x of
     InstRound instRound -> Prelude'.Just instRound
     _ -> Prelude'.Nothing

instance P'.Default InstType where
  defaultValue = InstCont P'.defaultValue

instance P'.Mergeable InstType