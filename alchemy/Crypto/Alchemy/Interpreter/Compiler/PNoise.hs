{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
--{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Crypto.Alchemy.Interpreter.Compiler.PNoise where

import Algebra.Additive as Additive (C)
import Algebra.Ring as Ring (C)
import Language.Haskell.TH
import Data.Functor.Trans.Tagged
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Type.Natural
import qualified GHC.TypeLits as TL

import Crypto.Lol.Reflects
import Crypto.Lol.Types.Unsafe.ZqBasic

-- | A value tagged by @pNoise =~ -log(noise rate)@.
newtype PNoise (h :: Nat) a = PN a deriving (Additive.C, Ring.C)

-- open, to be defined possibly for other Zq reprs
type family Modulus zq :: k
type instance Modulus (ZqBasic q i) = q

-- Maps PNoise to a modulus from a list, according to a heuristic

-- turns a list of moduli into a nested-pair representation
type family List2Zq zqs where
  List2Zq '[zq0] = zq0
  List2Zq (zq0 ': zqs) = (zq0, List2Zq zqs)

type family ZqsToMods zqs where
  ZqsToMods '[] = '[]
  ZqsToMods (x ': xs) = (Modulus x) ': (ZqsToMods xs)

data PNoiseMod = PNM TL.Nat Nat

-- | Turn a list of PNMs into a list of Nats to be operated on by singletons
type family LogOnly zqs where
  LogOnly '[] = '[]
  LogOnly (('PNM q pq) ': qs) = pq ': LogOnly qs

singletons [d|
            -- | Version included with singletons takes a TypeLit, rather than Nat
            take :: Nat -> [a] -> [a]
            take Z _ = []
            take (S n) (x : xs) = x : take n xs

            -- given a list of Nats representing the pNoise of moduli and an `h`,
            -- output the number of moduli in the list to use
            -- EAC: Singletons doesn't like guard patterns
            pNoise2Len :: [Nat] -> Nat -> Nat
            pNoise2Len [] _ = error "Requested h is too largle!"
            pNoise2Len (pq : pqs) h =
              if h <= pq
              then S Z
              else S (pNoise2Len pqs h)
           |]

type PNoise2Zq zqs h =
  -- compute the *number* of moduli needed, restrict the list to that many,
  -- *reverse* it, so that when we build a modulus out of it, it's in the optimal order for RescaleCyc
  List2Zq (Reverse (Take (PNoise2Len (LogOnly (ZqsToMods zqs)) h) zqs))


-- 8 bits seems like too much, because our moduli need to be slightly
-- less than 32 bits to fit into a 64 bit modulus,
-- but we'd still like to treat them as having 4 units of noise.
-- | One unit of "noise", in bits.
noiseUnit :: Double
noiseUnit = 7.5

mkTypeNat :: Int -> TypeQ
mkTypeNat 0 = conT 'Z
mkTypeNat x = conT 'S `appT` (mkTypeNat $ x-1)

mkTypeLit :: Integer -> TypeQ
mkTypeLit = litT . numTyLit

mkQ :: Integer -> TypeQ
mkQ q =
  let lgq = floor $ logBase 2 (fromInteger q :: Double) / noiseUnit
  in conT 'PNM `appT` (mkTypeLit q) `appT` (mkTypeNat lgq)

instance (Reflects x i) => Reflects ('PNM x y) i where
  value = retag $ value @x