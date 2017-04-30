{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Alchemy.Interpreter.Compiler.PNoise 
-- CJP: need an explicit export list?
where

import           Algebra.Additive          as Additive (C)
import           Algebra.Ring              as Ring (C)
import           Data.Functor.Trans.Tagged
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           Data.Type.Natural
import qualified GHC.TypeLits              as TL
import           Language.Haskell.TH

import Crypto.Lol.Reflects
import Crypto.Lol.Types.Unsafe.ZqBasic

-- | A value tagged by @pNoise =~ -log(noise rate)@.
newtype PNoise (h :: Nat) a = PN a
  deriving (Additive.C, Ring.C)

-- CJP: why should this be defined here?
type family Modulus zq :: k
type instance Modulus (ZqBasic q i) = q

-- | Convert a list to nested pairs, in "reverse inside out" order:
-- last element of list is first and outermost.
type family List2Pairs a where List2Pairs a = L2P (Reverse a)

-- auxiliary family
type family L2P a where
  L2P '[a] = a
  L2P (a ': b) = (a, List2Pairs b)

-- CJP: can this ad-hoc MapModulus be eliminated by using singletons
-- type-level Map?

-- | Maps  'Modulus' over a type list.
type family MapModulus zqs where
  MapModulus '[] = '[]
  MapModulus (x ': xs) = (Modulus x) ': (MapModulus xs)

-- | A type-lit 'TL.Nat' with a type-natural 'Nat'.
data TLNatNat = NN TL.Nat Nat

type family NatOf a where
  NatOf ('NN a b) = b

-- CJP: ditto: use singletons type-level Map?

-- | Maps 'NatOf' over a type list.
type family MapNatOf a where
  MapNatOf '[] = '[]
  MapNatOf (a ': rest) = (NatOf a) ': MapNatOf rest

singletons [d|
             -- | (Singletons version takes a TypeLit, rather than a 'Nat'.)
             take :: Nat -> [a] -> [a]
             take Z _ = []
             take (S n) (x : xs) = x : take n xs

             -- | Given a list and a threshold h, output the length of
             -- the shortest nonempty prefix whose sum is >= h.

             -- CJP: fixed a bug here -- it was never decreasing h!
             prefixLen :: [Nat] -> Nat -> Nat
             prefixLen (a : rest) h = if a >= h then S Z
                                      else S (prefixLen rest (h :- a))
             prefixLen [] _ = 
               error "prefixLen: threshold is larger than sum of list entries"
           |]

-- | Given a list of moduli, construct nested pairs representing a
-- modulus large enough to contain the given pNoise @h@.
type PNoise2Zq zqs h =
  List2Pairs (Take (PrefixLen (MapNatOf (MapModulus zqs)) h) zqs)


-- 8 bits seems like too much, because our moduli need to be slightly
-- less than 32 bits to fit into a 64 bit modulus,
-- but we'd still like to treat them as having 4 units of noise.

-- | Base of the logarithm in pNoise := -log (noise rate).
pNoiseBase :: Double
pNoiseBase = 7.5

mkTypeNat :: Int -> TypeQ
mkTypeNat x | x < 0 = error $ "mkTypeNat: negative argument " ++ show x
mkTypeNat 0 = conT 'Z
mkTypeNat x = conT 'S `appT` (mkTypeNat $ x-1)

mkTypeLit :: Integer -> TypeQ
mkTypeLit = litT . numTyLit

mkQ :: Integer -> TypeQ
mkQ q =
  let lgq = floor $ logBase pNoiseBase (fromInteger q)
  in conT 'NN `appT` (mkTypeLit q) `appT` (mkTypeNat lgq)

instance (Reflects x i) => Reflects ('NN x y) i where
  value = retag $ value @x

