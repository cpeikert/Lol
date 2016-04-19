{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Challenges.Proto.RLWE.Instance.InstType where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Challenges.Proto.RLWE.RLWEInstanceCont as RLWE (RLWEInstanceCont)
import qualified Challenges.Proto.RLWE.RLWEInstanceDisc as RLWE (RLWEInstanceDisc)
import qualified Challenges.Proto.RLWE.RLWRInstance as RLWE (RLWRInstance)

data InstType = RlweInstCont{rlweInstCont :: (RLWE.RLWEInstanceCont)}
              | RlweInstDisc{rlweInstDisc :: (RLWE.RLWEInstanceDisc)}
              | RlwrInst{rlwrInst :: (RLWE.RLWRInstance)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)
get'rlweInstCont x
 = case x of
     RlweInstCont rlweInstCont -> Prelude'.Just rlweInstCont
     _ -> Prelude'.Nothing
get'rlweInstDisc x
 = case x of
     RlweInstDisc rlweInstDisc -> Prelude'.Just rlweInstDisc
     _ -> Prelude'.Nothing
get'rlwrInst x
 = case x of
     RlwrInst rlwrInst -> Prelude'.Just rlwrInst
     _ -> Prelude'.Nothing

instance P'.Default InstType where
  defaultValue = RlweInstCont P'.defaultValue

instance P'.Mergeable InstType