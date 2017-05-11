{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Proto.RLWE.Challenges.Challenge.Params where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Proto.RLWE.Challenges.ContParams as Crypto.Proto.RLWE.Challenges (ContParams)
import qualified Crypto.Proto.RLWE.Challenges.DiscParams as Crypto.Proto.RLWE.Challenges (DiscParams)
import qualified Crypto.Proto.RLWE.Challenges.RLWRParams as Crypto.Proto.RLWE.Challenges (RLWRParams)

data Params = Cparams{cparams :: (Crypto.Proto.RLWE.Challenges.ContParams)}
            | Dparams{dparams :: (Crypto.Proto.RLWE.Challenges.DiscParams)}
            | Rparams{rparams :: (Crypto.Proto.RLWE.Challenges.RLWRParams)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)
get'cparams x
 = case x of
     Cparams cparams -> Prelude'.Just cparams
     _ -> Prelude'.Nothing
get'dparams x
 = case x of
     Dparams dparams -> Prelude'.Just dparams
     _ -> Prelude'.Nothing
get'rparams x
 = case x of
     Rparams rparams -> Prelude'.Just rparams
     _ -> Prelude'.Nothing

instance P'.Default Params where
  defaultValue = Cparams P'.defaultValue

instance P'.Mergeable Params