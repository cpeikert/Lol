{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, GADTs,
             InstanceSigs, KindSignatures, PolyKinds, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- An implementation of the ring-LWE key-homomorphic PRF from [BP14].

-- TODO: change module to Crypto.Lol.Applications.KeyHomomorphicPRF.
--       llvm is not functioning.
module Crypto.Lol.KeyHomomorphicPRF
( 
) where

import Crypto.Lol.FullTree
import Crypto.Lol.PosBinDefs
import Crypto.Lol.SafeMatrix

import Crypto.Lol.Types.Numeric

-- SafeMatrix may require a scalar multiplication (for s * a_T(x)) (not difficult)
-- Equation (2.10) in [BP14]
--computePRF :: FullTree (n :: Pos) Bool (SafeMatrix a) -> -- ^ Full tree T
--              Int -> -- ^ modulus p
--              Int -> -- ^ secret s
--              SafeMatrix a
--computePrf a0 a1 t p s = map (rescale (s * (computeVector a0 a1 t)) :: ZqBasic p Int)
