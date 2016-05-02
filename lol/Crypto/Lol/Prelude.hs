{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, FunctionalDependencies,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RankNTypes, RebindableSyntax,
             ScopedTypeVariables, StandaloneDeriving, TemplateHaskell,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | A substitute for the Prelude that is more suitable for Lol.  This
-- module exports most of the Numeric Prelude and other frequently
-- used modules, plus some low-level classes, missing instances, and
-- assorted utility functions.

module Crypto.Lol.Prelude
(
-- * Classes and families
  Enumerable(..)
, Mod(..)
, Subgroup(..)
, Reduce(..), LiftOf, Lift, Lift'(..), Rescale(..), Encode(..), msdToLSD
, CharOf
-- * Numeric
, module Crypto.Lol.Types.Numeric
-- * Complex
, module Crypto.Lol.Types.Complex
-- * Factored
, module Crypto.Lol.Factored
-- * Miscellaneous
, rescaleMod, roundCoset
, fromJust', pureT, peelT, pasteT, withWitness, withWitnessT
, module Data.Functor.Trans.Tagged
, module Data.Proxy
) where

import Crypto.Lol.Factored
import Crypto.Lol.Types.Complex
import Crypto.Lol.Types.Numeric

import Algebra.Field          as Field (C)
import Algebra.IntegralDomain as IntegralDomain (C)
import Algebra.Ring           as Ring (C)

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Monad.Identity
import Control.Monad.Random
import Data.Coerce
import Data.Default
import Data.Functor.Trans.Tagged
import Data.Maybe
import Data.Proxy
import Data.Singletons

-- for Unbox instance of Maybe a
import qualified Data.Vector.Unboxed          as U
import           Data.Vector.Unboxed.Deriving

instance NFData (Proxy (a :: k)) where rnf Proxy = ()

deriving instance NFData (m a) => NFData (TaggedT s m a)
deriving instance (MonadRandom m) => MonadRandom (TaggedT (tag :: k) m)

derivingUnbox "Maybe"
  [t| forall a . (Default a, U.Unbox a) => Maybe a -> (Bool, a) |]
  [| maybe (False, def) (\ x -> (True, x)) |]
  [| \ (b, x) -> if b then Just x else Nothing |]

instance Default Bool where def = False

-- | The characteristic of a ring, represented as a type.
type family CharOf fp :: k

-- | Poor man's 'Enum'.
class Enumerable a where
  values :: [a]

-- | Represents a quotient group modulo some integer.
class (ToInteger (ModRep a), Additive a) => Mod a where
  type ModRep a
  modulus :: Tagged a (ModRep a)

-- | Represents that @a@ is a subgroup of @b@.
class (Additive a, Additive b) => Subgroup a b where
  fromSubgroup :: a -> b

-- | Represents that @b@ is a quotient group of @a@.
class (Additive a, Additive b) => Reduce a b where
  reduce :: a -> b

-- | Represents that @b@ can be lifted to a "short" @a@ congruent to @b@.
type Lift b a = (Lift' b, LiftOf b ~ a)

-- | The type of representatives of @b@.
type family LiftOf b

-- | Fun-dep version of Lift.
class (Reduce (LiftOf b) b) => Lift' b where
  lift :: b -> LiftOf b

-- | Represents that @a@ can be rescaled to @b@, as an "approximate"
-- additive homomorphism.
class (Additive a, Additive b) => Rescale a b where
  rescale :: a -> b

-- | Represents that the target ring can "noisily encode" values from
-- the source ring, in either "most significant digit" (MSD) or "least
-- significant digit" (LSD) encodings, and provides conversion factors
-- between the two types of encodings.

class (Field src, Field tgt) => Encode src tgt where
    -- | The factor that converts an element from LSD to MSD encoding
    -- in the target field, with associated scale factor to apply to
    -- correct the resulting encoded value.
    lsdToMSD :: (src, tgt)

-- | Inverted entries of 'lsdToMSD'.
msdToLSD :: (Encode src tgt) => (src, tgt)
msdToLSD = (recip *** recip) lsdToMSD
{-# INLINABLE msdToLSD #-}

-- | A default implementation of rescaling for 'Mod' types.
rescaleMod :: forall a b .
              (Mod a, Mod b, (ModRep a) ~ (ModRep b),
               Lift a (ModRep b), Ring b)
              => a -> b
{-# INLINABLE rescaleMod #-}
rescaleMod =
    let qval = proxy modulus (Proxy :: Proxy a)
        q'val = proxy modulus (Proxy :: Proxy b)
    in \x -> let (quot',_) = divModCent (q'val * lift x) qval
             in fromIntegral quot'

-- | Deterministically round to a nearby value in the desired coset
roundCoset :: forall zp z r .
              (Mod zp, z ~ ModRep zp, Lift zp z, RealField r) => zp -> r -> z
{-# INLINABLE roundCoset #-}
roundCoset = let pval = proxy modulus (Proxy::Proxy zp)
             in \ zp x -> let rep = lift zp
                          in rep + roundMult pval (x - fromIntegral rep)

---------- Instances for product groups/rings ----------

type instance LiftOf (a,b) = Integer

instance (Mod a, Mod b, Lift' a, Lift' b, Reduce Integer (a,b),
          ToInteger (LiftOf a), ToInteger (LiftOf b))
         => Lift' (a,b) where

  {-# INLINABLE lift #-}
  lift (a,b) =
    let moda = toInteger $ proxy modulus (Proxy::Proxy a)
        modb = toInteger $ proxy modulus (Proxy::Proxy b)
        q = moda * modb
        ainv = fromMaybe (error "Lift' (a,b): moduli not coprime") $ moda `modinv` modb
        lifta = toInteger $ lift a
        liftb = toInteger $ lift b
        -- put in [-q/2, q/2)
        (_,r) = (moda * (liftb - lifta) * ainv + lifta) `divModCent` q
    in r


-- NP should define Ring and Field instances for pairs, but doesn't.
-- So we do it here.
instance (Ring r1, Ring r2) => Ring.C (r1, r2) where
  (x1, x2) * (y1, y2) = (x1*y1, x2*y2)
  one = (one,one)
  fromInteger x = (fromInteger x, fromInteger x)

  {-# INLINABLE (*) #-}
  {-# INLINABLE one #-}
  {-# INLINABLE fromInteger #-}

instance (Field f1, Field f2) => Field.C (f1, f2) where
  (x1, x2) / (y1, y2) = (x1 / y1, x2 / y2)
  recip = recip *** recip
  {-# INLINABLE (/) #-}
  {-# INLINABLE recip #-}

instance (IntegralDomain a, IntegralDomain b) => IntegralDomain.C (a,b) where
  (a1,b1) `divMod` (a2,b2) =
    let (da,ra) = (a1 `divMod` a2)
        (db,rb) = (b1 `divMod` b2)
    in ((da,db), (ra,rb))
  {-# INLINABLE divMod #-}

instance (Mod a, Mod b) => Mod (a,b) where
  type ModRep (a,b) = Integer

  modulus = tag $ fromIntegral (proxy modulus (Proxy::Proxy a)) *
            fromIntegral (proxy modulus (Proxy::Proxy b))
  {-# INLINABLE modulus #-}

instance (Reduce a b1, Reduce a b2) => Reduce a (b1, b2) where
  reduce x = (reduce x, reduce x)
  {-# INLINABLE reduce #-}

-- instances of Rescale for a product
instance (Mod a, Field b, Lift a (ModRep a), Reduce (LiftOf a) b)
         => Rescale (a,b) b where
  rescale = let q1val = proxy modulus (Proxy::Proxy a)
                q1inv = recip $ reduce q1val
            in \(x1,x2) -> q1inv * (x2 - reduce (lift x1))
  {-# INLINABLE rescale #-}

instance (Mod b, Field a, Lift b (ModRep b), Reduce (LiftOf b) a)
         => Rescale (a,b) a where
  rescale = let q2val = proxy modulus (Proxy::Proxy b)
                q2inv = recip $ reduce q2val
            in \(x1,x2) -> q2inv * (x1 - reduce (lift x2))
  {-# INLINABLE rescale #-}

-- some multi-step scaledowns; could do this forever
instance (Rescale (a,(b,c)) (b,c), Rescale (b,c) c)
         => Rescale (a,(b,c)) c where
  rescale = (rescale :: (b,c) -> c) . rescale
  {-# INLINABLE rescale #-}

instance (Rescale ((a,b),c) (a,b), Rescale (a,b) a)
         => Rescale ((a,b),c) a where
  rescale = (rescale :: (a,b) -> a) . rescale
  {-# INLINABLE rescale #-}

-- scaling up to a product
instance (Ring a, Mod b, Reduce (ModRep b) a) => Rescale a (a,b) where
  -- multiply by q2
  rescale = let q2val = reduce $ proxy modulus (Proxy::Proxy b)
            in \x -> (q2val * x, zero)
  {-# INLINABLE rescale #-}

instance (Ring b, Mod a, Reduce (ModRep a) b) => Rescale b (a,b) where
  -- multiply by q1
  rescale = let q1val = reduce $ proxy modulus (Proxy::Proxy a)
            in \x -> (zero, q1val * x)
  {-# INLINABLE rescale #-}

-- Instance of 'Encode' for product ring.
instance (Encode s t1, Encode s t2, Field (t1, t2)) => Encode s (t1, t2) where
  {-# INLINABLE lsdToMSD #-}
  lsdToMSD = let (s1, t1conv) = lsdToMSD
                 (s2, t2conv) = lsdToMSD
             in (negate s1 * s2, (t1conv,t2conv))

-- Random could have defined this instance, but didn't, so we do it
-- here.
instance (Random a, Random b) => Random (a,b) where
  {-# INLINABLE random #-}
  random g = let (a,g') = random g
                 (b, g'') = random g'
             in ((a,b), g'')

  {-# INLINABLE randomR #-}
  randomR ((loa,lob), (hia,hib)) g = let (a,g') = randomR (loa,hia) g
                                         (b,g'') = randomR (lob,hib) g'
                                     in ((a,b),g'')

-- | Version of 'fromJust' with an error message.
fromJust' :: String -> Maybe a -> a
fromJust' str = fromMaybe (error str)

-- | Apply any applicative to a Tagged value.
pureT :: Applicative f => Tagged t a -> TaggedT t f a
pureT = mapTaggedT (pure . runIdentity)

-- | Expose the monad of a tagged value.
peelT :: Tagged t (f a) -> TaggedT t f a
peelT = coerce

-- | Hide the monad of a tagged value.
pasteT :: TaggedT t f a -> Tagged t (f a)
pasteT = coerce

-- | Use a singleton as a witness to extract a value from a tagged value.
withWitness :: forall n r . (SingI n => Tagged n r) -> Sing n -> r
withWitness t wit = withSingI wit $ proxy t (Proxy::Proxy n)
{-# INLINABLE withWitness #-}

-- | Transformer version of 'withWitness'.
withWitnessT :: forall n mon r .
                (SingI n => TaggedT n mon r) -> Sing n -> mon r
withWitnessT t wit = withSingI wit $ proxyT t (Proxy::Proxy n)
{-# INLINABLE withWitnessT #-}
