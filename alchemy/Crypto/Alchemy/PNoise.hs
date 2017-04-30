{-# LANGUAGE DataKinds, FlexibleInstances, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Crypto.Alchemy.PNoise where

import Algebra.Ring as Ring
import Language.Haskell.TH
import Crypto.Lol.Reflects
import GHC.TypeLits
import Data.Proxy
import Data.Functor.Trans.Tagged


data NoiseCapacity = NC Integer Integer

addNoiseCapacity :: Integer -> NoiseCapacity
addNoiseCapacity q =
  let lgq = ceiling $ logBase 2 (Ring.fromInteger q :: Double)
  in NC q lgq

-- EAC: Don't want to use pairs here, we need our own data type. Not sure how to do it though.
promoteNC :: NoiseCapacity -> TypeQ
promoteNC (NC x y) = promotedTupleT 2 `appT` (litT $ numTyLit x) `appT` (litT $ numTyLit y)

--http://stackoverflow.com/questions/36115800/type-synonyms-with-templatehaskell
--https://github.com/cpeikert/Lol/commits/master?after=70b106cd0c4e993965ec054162269874ec2cbeb4+629
--http://stackoverflow.com/questions/36062614/typelit-from-generic-integer-expression

mkQ = promoteNC . addNoiseCapacity

--
instance (KnownNat x, Ring.C i) => Reflects '( (x :: Nat), (y :: Nat) ) i where
  value = tag $ Ring.fromInteger $ natVal (Proxy::Proxy x)