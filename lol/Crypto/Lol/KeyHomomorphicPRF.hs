{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, GADTs,
             InstanceSigs, KindSignatures, PolyKinds, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- An implementation of the ring-LWE key-homomorphic PRF from [BP14].

-- TODO: change module to Crypto.Lol.Applications.KeyHomomorphicPRF.
--       llvm is not functioning.
module Crypto.Lol.KeyHomomorphicPRF
( computePRF
, uComputePRF
) where

import Crypto.Lol.FullTree as F
import Crypto.Lol.LatticePrelude
import Crypto.Lol.PosBinDefs
import Crypto.Lol.UFullTree as U

import Crypto.Lol.Types.Numeric as N

-- Equation (2.10) in [BP14].
computePRF :: (Ring a, Ring b, Rescale a b) =>
              FullTree n l (MMatrix a) -> -- ^ Full tree T
              a -> -- ^ secret s
              MMatrix b
computePRF t s = fmap (rescale . (N.*s)) (F.rootValue t)

uComputePRF :: (Ring a, Ring b, Rescale a b) =>
              UFullTree Int Int l (MMatrix a) -> -- ^ Full tree T
              a -> -- ^ secret s
              MMatrix b
uComputePRF t s = fmap (rescale . (N.*s)) (U.rootValue t)
