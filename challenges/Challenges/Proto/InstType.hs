{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Challenges.Proto.InstType where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Challenges.Proto.ContLWEInstance as Lwe (ContLWEInstance)
import qualified Challenges.Proto.DiscLWEInstance as Lwe (DiscLWEInstance)
import qualified Challenges.Proto.LWRInstance as Lwe (LWRInstance)

data InstType = Clweinst{clweinst :: (Lwe.ContLWEInstance)}
              | Dlweinst{dlweinst :: (Lwe.DiscLWEInstance)}
              | Lwrinst{lwrinst :: (Lwe.LWRInstance)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
get'clweinst x
 = case x of
     Clweinst clweinst -> Prelude'.Just clweinst
     _ -> Prelude'.Nothing
get'dlweinst x
 = case x of
     Dlweinst dlweinst -> Prelude'.Just dlweinst
     _ -> Prelude'.Nothing
get'lwrinst x
 = case x of
     Lwrinst lwrinst -> Prelude'.Just lwrinst
     _ -> Prelude'.Nothing

instance P'.Default InstType where
  defaultValue = Clweinst P'.defaultValue

instance P'.Mergeable InstType