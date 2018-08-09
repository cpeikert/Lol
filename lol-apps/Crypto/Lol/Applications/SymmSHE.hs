{-|
Module      : Crypto.Lol.Applications.SymmSHE
Description : Symmetric-key homomorphic encryption.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\O{\mathcal{O}} \)

Symmetric-key somewhat homomorphic encryption.  See Section 4 of
<http://eprint.iacr.org/2015/1134> for mathematical description.
-}

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Lol.Applications.SymmSHE
(
-- * Data types
SK, PT, CT -- don't export constructors!
-- * Keygen, encryption, decryption
, genSK, genAnotherSK
, encrypt
, errorTerm, decrypt
-- * Arithmetic with public values
, addPublic, mulPublic
-- * Modulus switching
, modSwitch, modSwitchPT
-- * Key switching
, KSHint
, ksLinearHint, ksQuadCircHint
, keySwitchLinear, keySwitchQuadCirc
-- * Ring switching
, embedSK, embedCT, twaceCT
, TunnelHint, tunnelHint
, tunnel
-- * Constraint synonyms
, GenSKCtx, EncryptCtx, ToSDCtx
, ErrorTermCtx, DecryptCtx
, AddPublicCtx, MulPublicCtx, ModSwitchPTCtx
, KSHintCtx, KeySwitchCtx
, TunnelHintCtx, TunnelCtx
, SwitchCtx, LWECtx -- these are internal, but exported for better docs
) where

import qualified Algebra.Additive as Additive (C)
import qualified Algebra.Ring     as Ring (C)

import Crypto.Lol             hiding (sin)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto

import           Crypto.Proto.Lol.R            (R)
import           Crypto.Proto.Lol.RqProduct    (RqProduct)
import qualified Crypto.Proto.SHE.KSHint       as P
import qualified Crypto.Proto.SHE.RqPolynomial as P
import qualified Crypto.Proto.SHE.SecretKey    as P
import qualified Crypto.Proto.SHE.TunnelHint   as P

import Control.Applicative  hiding ((*>))
import Control.DeepSeq
import Control.Monad        as CM
import Control.Monad.Random hiding (lift)
import Data.Maybe
import Data.Traversable     as DT
import Data.Typeable

import MathObj.Polynomial as P

-- | secret key
data SK r where
  SK  :: (ToRational v, NFData v) => v -> r -> SK r

-- | plaintext
type PT rp = rp

-- | Ciphertext encoding type
data Encoding = MSD | LSD deriving (Show, Eq)

-- | Ciphertext over \( R'_q \) encrypting a plaintext in \( R_p \)\,
-- where \( R=\mathcal{O}_m \).
data CT m zp r'q =
  CT
  !Encoding                     -- MSD/LSD encoding
  !Int                          -- accumulated power of g_m' in c(s)
  !zp                           -- factor to mul by upon decryption
  !(Polynomial r'q)             -- the polynomial c(s)
  deriving (Show)

-- Note: do *not* give an Eq instance for CT, because it's not
-- meaningful to compare ciphertexts for equality

---------- Basic functions: Gen, Enc, Dec ----------

-- | Constraint synonym for generating a secret key.
type GenSKCtx c m z v =
  (ToInteger z, RoundedGaussianCyc (c m) z, ToRational v, NFData v)

-- | Generates a secret key with (index-independent) scaled variance
-- parameter \( v \); see 'roundedGaussian'.
genSK :: (GenSKCtx c m z v, MonadRandom rnd) => v -> rnd (SK (c m z))
genSK v = liftM (SK v) $ roundedGaussian v

-- | Generates a secret key with the same scaled variance
-- as the given secret key.
genAnotherSK :: (GenSKCtx c m z a, MonadRandom rnd) => SK a -> rnd (SK (c m z))
genAnotherSK (SK v _) = genSK v

-- | Constraint synonym for encryption.
type EncryptCtx c m m' z zp zq =
  (Ring zp, Cyclotomic (c m' zq), Ring (c m' zq), Random (c m' zq),
   Reduce (c m' z) (c m' zq), Reduce (LiftOf (c m' zp)) (c m' zq),
   CosetGaussianCyc (c m' zp), ExtensionCyc c zp,  m `Divides` m')

-- | Encrypt a plaintext under a secret key.
encrypt :: forall c m m' z zp zq rnd .
  (EncryptCtx c m m' z zp zq, MonadRandom rnd)
  => SK (c m' z) -> PT (c m zp) -> rnd (CT m zp (c m' zq))
encrypt (SK svar s) =
  let sq = adviseCRT $ reduce s
  in \pt -> do
    e <- cosetGaussian svar (embed pt :: c m' zp)
    c1 <- getRandom
    return $! CT LSD zero one $ fromCoeffs [reduce e - c1 * sq, c1]

-- | Constraint synonym for extracting the error term of a ciphertext.
type ErrorTermCtx c m' z zp zq =
  (ToSDCtx c m' zp zq, Ring (c m' zq), Reduce (c m' z) (c m' zq), LiftCyc (c m') zq)

-- | Extract the error term of a ciphertext.
errorTerm :: ErrorTermCtx c m' z zp zq
          => SK (c m' z) -> CT m zp (c m' zq) -> c m' (LiftOf zq)
errorTerm (SK _ s) = let sq = reduce s in
  \ct -> let (CT LSD _ _ c) = toLSD ct
         in liftDec $ evaluate c sq

-- for when we know the division must succeed
divG' :: (Cyclotomic c) => c -> c
divG' = fromJust . divG

-- | Constraint synonym for decryption.
type DecryptCtx c m m' z zp zq =
  (ErrorTermCtx c m' z zp zq, Cyclotomic (c m' zp), Module zp (c m zp),
   Reduce (c m' (LiftOf zq)) (c m' zp), ExtensionCyc c zp, m `Divides` m')

-- | Decrypt a ciphertext.
decrypt :: forall c m m' z zp zq . DecryptCtx c m m' z zp zq
           => SK (c m' z) -> CT m zp (c m' zq) -> PT (c m zp)
decrypt sk ct =
  let ct'@(CT LSD k l _) = toLSD ct
  in let e :: c m' zp = reduce $ errorTerm sk ct'
     in l *> twace (iterate divG' e !! k)

---------- LSD/MSD switching ----------

-- | Constraint synonym for converting between ciphertext encodings.
type ToSDCtx c m' zp zq =
  (Encode zp zq, Cyclotomic (c m' zq), Ring (c m' zq), Module zq (c m' zq))

toLSD, toMSD :: forall c m m' zp zq .
  ToSDCtx c m' zp zq => CT m zp (c m' zq) -> CT m zp (c m' zq)

-- CJP: reduce duplication in these functions?  They differ in only two places

-- | Convert a ciphertext to MSD encoding.
toMSD = let (zpScale, zqScale :: zq) = lsdToMSD
        in \ct@(CT enc k l c) -> case enc of
          MSD -> ct
          LSD -> CT MSD k (zpScale * l) ((zqScale *>) <$> c)

-- | Convert a ciphertext to LSD encoding.
toLSD = let (zpScale, zqScale :: zq) = msdToLSD
        in \ct@(CT enc k l c) -> case enc of
          LSD -> ct
          MSD -> CT LSD k (zpScale * l) ((zqScale *>) <$> c)

---------- Modulus switching ----------

-- | Rescale a polynomial in MSD encoding, for best noise behavior.
modSwitchMSD :: (RescaleCyc (c m') zq zq') => Polynomial (c m' zq) -> Polynomial (c m' zq')
modSwitchMSD c = case coeffs c of
  []    -> fromCoeffs []
  c0:c' -> fromCoeffs $ rescaleDec c0 : map rescalePow c'

-- | Rescale a ciphertext to a new modulus.
modSwitch :: (RescaleCyc (c m') zq zq', ToSDCtx c m' zp zq) => CT m zp (c m' zq) -> CT m zp (c m' zq')
modSwitch ct = let CT MSD k l c = toMSD ct
               in CT MSD k l $ modSwitchMSD c

-- | Constraint synonym for modulus switching.
type ModSwitchPTCtx c m' zp zp' zq = (Lift' zp, Reduce (LiftOf zp) zp', ToSDCtx c m' zp zq)

-- | Homomorphically divide a plaintext that is known to be a multiple
-- of \( (p/p') \) by that factor, thereby scaling the plaintext modulus
-- from \( p \) to \( p' \).
modSwitchPT :: ModSwitchPTCtx c m' zp zp' zq => CT m zp (c m' zq) -> CT m zp' (c m' zq)
modSwitchPT ct = let CT MSD k l c = toMSD ct in
    CT MSD k (reduce (lift l)) c

---------- Key switching ----------

-- | Constraint synonym for generating a ring-LWE sample.
type LWECtx c m' z zq =
  (Cyclotomic (c m' zq), RoundedGaussianCyc (c m') z, Reduce (c m' z) (c m' zq),
   Random (c m' zq), Ring (c m' zq))

-- An LWE sample for a given secret (corresponding to a linear
-- ciphertext encrypting 0 in MSD form)
lweSample :: (LWECtx c m' z zq, MonadRandom rnd)
             => SK (c m' z) -> rnd (Polynomial (c m' zq))
lweSample (SK svar s) =
  -- adviseCRT because we call `replicateM (lweSample s)` below, but only want to do CRT once.
  let sq = adviseCRT $ negate $ reduce s
  in do
    e <- roundedGaussian svar
    c1 <- adviseCRT <$> getRandom -- want entire hint to be in CRT form
    return $ fromCoeffs [c1 * sq + reduce (e `asTypeOf` s), c1]

-- | Key-switch hint.
newtype KSHint gad r'q' = KSHint [Polynomial r'q']
  deriving (NFData)

-- | Constraint synonym for generating key-switch hints.
type KSHintCtx gad c m' z zq = (LWECtx c m' z zq, Gadget gad (c m' zq))

-- | Helper function that generates a hint that "encrypts" a value
-- under a secret key, in the sense required for key-switching.  The
-- hint works for any plaintext modulus, but must be applied on a
-- ciphertext in MSD form.
ksHint :: forall gad c m' z zq rnd . (KSHintCtx gad c m' z zq, MonadRandom rnd)
          => SK (c m' z) -> c m' z -> rnd (KSHint gad (c m' zq))
ksHint skout val = do -- rnd monad
  let valgad = encode @gad $ reduce val
  samples <- replicateM (length valgad) (lweSample skout)
  return $ KSHint $ zipWith (+) (P.const <$> valgad) samples

-- | A hint to switch a linear ciphertext under \( s_{\text{in}} \) to
-- a linear one under \( s_{\text{out}} \).
ksLinearHint :: forall gad c m' z zq' rnd . (KSHintCtx gad c m' z zq', MonadRandom rnd)
  => SK (c m' z) -- sout
  -> SK (c m' z) -- sin
  -> rnd (KSHint gad (c m' zq'))
ksLinearHint skout (SK _ sin) = ksHint skout sin

-- | A hint to switch a quadratic ciphertext to a linear
-- one under the same key.
ksQuadCircHint :: forall gad c m' z zq' rnd .
  (KSHintCtx gad c m' z zq', Ring (c m' z), MonadRandom rnd)
  => SK (c m' z) -> rnd (KSHint gad (c m' zq'))
ksQuadCircHint sk@(SK _ s) = ksHint sk (s*s)

-- poor man's module multiplication for knapsack
(*>>) :: (Ring r, Functor f) => r -> f r -> f r
(*>>) r = fmap (r *)

knapsack :: (r'q ~ c m' zq, Cyclotomic (c m' zq), Ring (c m' zq))
            => [Polynomial r'q] -> [r'q] -> Polynomial r'q
-- adviseCRT here because we map (x *) onto each polynomial coeff
knapsack hint xs = sum $ zipWith (*>>) (adviseCRT <$> xs) hint

-- | Constraint synonym for applying a key-switch hint.
type SwitchCtx gad c m' zq =
  (Cyclotomic (c m' zq), Ring (c m' zq), Decompose gad (c m' zq),
   Reduce (DecompOf (c m' zq)) (c m' zq))

-- Helper function: applies key-switch hint to a ring element.
keySwitch :: forall gad c m' zq r'q . (SwitchCtx gad c m' zq, r'q ~ c m' zq)
             => KSHint gad r'q -> r'q -> Polynomial r'q
keySwitch (KSHint hint) c = knapsack hint $ reduce <$> decompose @gad c

-- | Constraint synonym for key switching.
type KeySwitchCtx gad c m' zp zq' =
  (ToSDCtx c m' zp zq', SwitchCtx gad c m' zq')

-- | Switch a linear ciphertext using the supplied hint.  (The input
-- ciphertext may first need to be rescaled so that its modulus
-- matches that of the hint.)
keySwitchLinear :: forall gad c m m' zp zq' . (KeySwitchCtx gad c m' zp zq')
  => KSHint gad (c m' zq')
  -> CT m zp (c m' zq')
  -> CT m zp (c m' zq')
keySwitchLinear hint ct =
  let CT MSD k l c = toMSD ct
  in case coeffs c of
       []      -> ct
       [_]     -> ct
       [c0,c1] -> CT MSD k l $ P.const c0 + keySwitch hint c1

-- | Switch a ciphertext of degree two or less (i.e., one with no more
-- than three components) to a ciphertext of degree one (or less)
-- under the /same/ key, using the supplied hint.  (The input
-- ciphertext may first need to be rescaled so that its modulus
-- matches that of the hint.)
keySwitchQuadCirc :: forall gad c m m' zp zq' . (KeySwitchCtx gad c m' zp zq')
  => KSHint gad (c m' zq')
  -> CT m zp (c m' zq')
  -> CT m zp (c m' zq')
keySwitchQuadCirc hint ct =
  let CT MSD k l c = toMSD ct
  in case coeffs c of
       []         -> ct
       [_]        -> ct
       [_,_]      -> ct
       [c0,c1,c2] -> CT MSD k l $ P.fromCoeffs [c0,c1] + keySwitch hint c2

---------- Misc homomorphic operations ----------

-- | Constraint synonym for adding a public value to an encrypted value.
type AddPublicCtx c m m' zp zq =
  (ToSDCtx c m' zp zq, Cyclotomic (c m zp), Module zp (c m zp),
   LiftCyc (c m) zp, Reduce (c m (LiftOf zp)) (c m zq),
   ExtensionCyc c zq, m `Divides` m')

-- | Homomorphically add a public \( R_p \) value to an encrypted
-- value.
addPublic :: forall c m m' zp zq . AddPublicCtx c m m' zp zq
          => c m zp -> CT m zp (c m' zq) -> CT m zp (c m' zq)
addPublic b ct = let CT LSD k l c = toLSD ct in
  let -- multiply public value by appropriate power of g and divide by the
      -- scale, to match the form of the ciphertext
      b' :: c m zq = reduce $ liftPow $ (recip l) *> (iterate mulG b !! k)
  in CT LSD k l $ c + P.const (embed b')

-- | Homomorphically multiply a public \(\mathbb{Z}_p\) value to an
-- encrypted value.
mulScalar :: forall zp zq c m m' .
  (Lift' zp, Reduce (LiftOf zp) zq, Module zq (c m' zq))
  => zp -> CT m zp (c m' zq) -> CT m zp (c m' zq)
mulScalar a (CT enc k l c) =
  let a' :: zq = reduce $ lift a
  in CT enc k l $ (a' *>) <$> c

-- | Constraint synonym for multiplying a public value with an encrypted value.
type MulPublicCtx c m m' zp zq =
  (LiftCyc (c m) zp, Reduce (c m (LiftOf zp)) (c m zq),
   ExtensionCyc c zq, m `Divides` m', Ring (c m' zq))

-- | Homomorphically multiply an encrypted value by a public \( R_p \)
-- value.
mulPublic :: forall c m m' zp zq . MulPublicCtx c m m' zp zq
             => c m zp -> CT m zp (c m' zq) -> CT m zp (c m' zq)
mulPublic a (CT enc k l r) =
  let a' = embed (reduce $ liftPow a :: c m zq)
  in CT enc k l $ (a' *) <$> r

-- | Increment the internal \( g \) exponent without changing the
-- encrypted message.
mulGCT :: (Cyclotomic r'q) => CT m zp r'q -> CT m zp r'q
mulGCT (CT enc k l c) = CT enc (k+1) l $ mulG <$> c

---------- NumericPrelude instances ----------

instance (Lift' zp, Reduce (LiftOf zp) zq, -- mulScalar
          ToSDCtx c m' zp zq, Eq zp, m `Divides` m')
         => Additive.C (CT m zp (c m' zq)) where

  zero = CT LSD 0 one zero

  -- the scales, g-exponents of ciphertexts, and MSD/LSD types must match.
  ct1@(CT enc1 k1 l1 c1) + ct2@(CT enc2 k2 l2 c2)
    | l1 /= l2 =
        let (CT enc' k' _ c') = mulScalar (l1*(recip l2)) ct1
        in (CT enc' k' l2 c') + ct2
    | k1 < k2 = iterate mulGCT ct1 !! (k2-k1) + ct2
    | k1 > k2 = ct1 + iterate mulGCT ct2 !! (k1-k2)
    | enc1 == LSD && enc2 == MSD = toMSD ct1 + ct2
    | enc1 == MSD && enc2 == LSD = ct1 + toMSD ct2
    | otherwise = CT enc1 k1 l1 $ c1 + c2

  negate (CT enc k l c) = CT enc k l $ negate <$> c

instance (ToSDCtx c m' zp zq, Additive (CT m zp (c m' zq)))
  => Ring.C (CT m zp (c m' zq)) where

  one = CT LSD 0 one one

  -- need at least one ct to be in LSD form
  ct1@(CT MSD _ _ _) * ct2@(CT MSD _ _ _) = toLSD ct1 * ct2

  -- first is in LSD
  (CT LSD k1 l1 c1) * (CT d2 k2 l2 c2) =
    -- mul by g so error maintains invariant: error*g is "round"
    CT d2 (k1+k2+1) (l1*l2) (mulG <$> c1 * c2)

  -- else, second must be in LSD
  ct1 * ct2 = ct2 * ct1

---------- Ring switching ----------

-- | Constraint synonym for 'absorbGFactors'.
type AbsorbGCtx c m' zp zq =
  (Ring (c m' zp), Ring (c m' zq), Cyclotomic (c m' zp), Cyclotomic (c m' zq),
   LiftCyc (c m') zp, Reduce (c m' (LiftOf zp)) (c m' zq))

-- | "Absorb" the powers of \( g \) associated with the ciphertext, at
-- the cost of some increase in noise. This is usually needed before
-- changing the index of the ciphertext ring.
absorbGFactors :: forall c zp zq m m' . AbsorbGCtx c m' zp zq
                  => CT m zp (c m' zq) -> CT m zp (c m' zq)
absorbGFactors ct@(CT enc k l r)
  | k == 0 = ct
  | k > 0 = let d :: c m' zp = iterate divG' one !! k
                rep = adviseCRT $ reduce $ liftPow d
            in CT enc 0 l $ (rep *) <$> r
  | otherwise = error "k < 0 in absorbGFactors"

-- | Embed a ciphertext in \( R' \) encrypting a plaintext in \( R \) to
-- a ciphertext in \( T' \) encrypting a plaintext in \( T \). The target
-- ciphertext ring \( T' \) must contain both the the source ciphertext
-- ring \( R' \) and the target plaintext ring \( T \).
embedCT :: (r `Divides` r', s `Divides` s', r `Divides` s, r' `Divides` s', ExtensionCyc c zq)
           => CT r zp (c r' zq) -> CT s zp (c s' zq)
-- We could call absorbGFactors first, insead of error.  Embedding
-- *essentially* maintains the invariant that noise*g is "round."
-- While g'/g can be non-spherical, it only stretches by at most a
-- factor of 2 per new odd prime.  We *cannot* multiply by g, then
-- embed, then divide by g' because the result would not remain in R'.
-- So this is the best we can do.
embedCT (CT d 0 l c) = CT d 0 l (embed <$> c)
embedCT _ = error "embedCT requires 0 factors of g; call aborbGFactors first"

-- | Embed a secret key from a subring into a superring.
embedSK :: (m `Divides` m', ExtensionCyc c z) => SK (c m z) -> SK (c m' z)
embedSK (SK v s) = SK v $ embed s

-- | "Tweaked trace" function for ciphertexts.  Mathematically, the
-- target plaintext ring \( S \) must contain the intersection of the
-- source plaintext ring \( T \) and the target ciphertext ring \( S'
-- \).  Here we make the stricter requirement that \( s = \gcd(s', t)
-- \).
twaceCT :: (r `Divides` r', s' `Divides` r', s ~ (FGCD s' r), ExtensionCyc c zq)
           => CT r zp (c r' zq) -> CT s zp (c s' zq)
-- we could call absorbGFactors first, insead of error
twaceCT (CT d 0 l c) = CT d 0 l (twace <$> c)
twaceCT _ = error "twaceCT requires 0 factors of g; call absorbGFactors first"

-- | Auxilliary data needed to tunnel from \(\O_{r'}\) to \(\O_{s'}\).
data TunnelHint gad c e r s e' r' s' zp zq =
  THint (Linear c e' r' s' zq) [KSHint gad (c s' zq)]

-- e' ~ (e * ...) is not needed in this module, but is at use sites...
-- | Constraint synonym for generating 'TunnelHint'.
type TunnelHintCtx c e r s e' r' s' z zp zq' gad =
  (ExtendLinCtx c e r s e' r' s' zp, -- extendLin
   e' ~ (e * (r' / r)),     -- convenience; implied by prev constraint
   Fact r, z ~ LiftOf zp,
   KSHintCtx gad c s' z zq',          -- ksHint
   LiftCyc (c s) zp,                  -- liftLin
   ExtensionCyc c z, e' `Divides` r', -- powBasis
   Reduce (c s' z) (c s' zq'),
   Cyclotomic (c r' z),         -- adviseCRT
   Ring (c r' z), Ring (c s' z), Random (c s' zq'), Gadget gad (c s' zq'))

-- | Generates auxilliary data needed to tunnel from \( \O_{r'} \) to
-- \( \O_{s'} \).
tunnelHint :: forall gad c e r s e' r' s' z zp zq' rnd .
  (MonadRandom rnd, TunnelHintCtx c e r s e' r' s' z zp zq' gad)
  => Linear c e r s zp
  -> SK (c s' z)
  -> SK (c r' z)
  -> rnd (TunnelHint gad c e r s e' r' s' zp zq')
tunnelHint f skout (SK _ sin) = -- generate hints
  (let f' = extendLin $ liftLin (Just Pow) f :: Linear c e' r' s' z
       -- choice of basis here must match coeffs* basis in tunnel (below)
       ps = proxy powBasis (Proxy::Proxy e')
       comps = (evalLin f' . (adviseCRT sin *)) <$> ps
   in THint (reduce f') <$> (CM.mapM (ksHint skout) comps))
  \\ lcmDivides @r @e'

-- | Constraint synonym for ring tunneling.
type TunnelCtx c r s e' r' s' zp zq' gad =
  (Fact r, Fact s, e' `Divides` r', e' `Divides` s', ExtensionCyc c zq', -- evalLin
   ToSDCtx c r' zp zq',                     -- toMSD
   AbsorbGCtx c r' zp zq',                  -- absorbGFactors
   SwitchCtx gad c s' zq')                  -- switch

-- | Homomorphically apply the \( E \)-linear function that maps the
-- elements of the decoding basis of \( R/E \) to the corresponding
-- \( S \)-elements in the input array.
tunnel :: forall gad c e r s e' r' s' zp zq' .
  (TunnelCtx c r s e' r' s' zp zq' gad)
  => TunnelHint gad c e r s e' r' s' zp zq'
  -> CT r zp (c r' zq')
  -> CT s zp (c s' zq')
tunnel (THint f'q hints) ct =
  (let CT MSD 0 s c = toMSD $ absorbGFactors ct
       [c0,c1] = coeffs c
       -- apply E-linear function to constant term c0
       c0' = evalLin f'q c0
       -- apply E-linear function to c1 via key-switching
       -- this basis must match the basis used above to generate the hints
       c1s = coeffsPow c1 :: [c e' zq']
       -- CJP: don't embed the c1s before decomposing them (inside
       -- switch); instead decompose in smaller ring before
       -- embedding (it matters).
       -- We may need to generalize switch or define an
       -- alternative.
       c1s' = zipWith keySwitch hints (embed <$> c1s)
       c1' = sum c1s'
   in CT MSD 0 s $ P.const c0' + c1')
    \\ lcmDivides @r @e'

---------- Utility instances ----------

instance (NFData zp, NFData r'q) => NFData (CT m zp r'q) where
  rnf (CT _ k sc cs) = rnf k `seq` rnf sc `seq` rnf cs

instance (NFData r) => NFData (SK r) where
  rnf (SK v s) = rnf v `seq` rnf s

instance (NFData (Linear c e' r' s' zq), NFData (c s' zq))
  => NFData (TunnelHint gad c e r s e' r' s' zp zq) where
  rnf (THint l t) = rnf l `seq` rnf t

instance Show r => Show (SK r) where
  show (SK v r) = "(SK " ++ (show $ toRational v) ++ " " ++ (show r) ++ ")"

----- Protoable instances

instance (Protoable r, ProtoType r ~ R) => Protoable (SK r) where
  type ProtoType (SK r) = P.SecretKey
  toProto (SK v r) = P.SecretKey (toProto r) (realToField v)
  fromProto (P.SecretKey r v) = (SK v) <$> fromProto r

instance (Protoable rq, ProtoType rq ~ RqProduct) => Protoable (Polynomial rq) where
  type ProtoType (Polynomial rq) = P.RqPolynomial
  toProto = P.RqPolynomial . toProto . coeffs
  fromProto (P.RqPolynomial x) = fromCoeffs <$> fromProto x

instance (Typeable gad, Protoable r'q', ProtoType r'q' ~ RqProduct)
  => Protoable (KSHint gad r'q') where
  type ProtoType (KSHint gad r'q') = P.KSHint

  toProto (KSHint cs) =
    P.KSHint
      (toProto cs)
      (toProto $ typeRepFingerprint $ typeRep (Proxy::Proxy gad))

  fromProto (P.KSHint poly gadrepr') = do
    let gadrepr = toProto $ typeRepFingerprint $ typeRep (Proxy::Proxy gad)
    if gadrepr == gadrepr'
    then KSHint <$> fromProto poly
    else error $ "Expected gadget " ++ (show $ typeRep (Proxy::Proxy gad))

instance (Mod zp, Typeable gad,
          Protoable (Linear c e' r' s' zq), Protoable (KSHint gad (c s' zq)),
          Reflects s Int, Reflects r Int, Reflects e Int)
  => Protoable (TunnelHint gad c e r s e' r' s' zp zq) where
  type ProtoType (TunnelHint gad c e r s e' r' s' zp zq) = P.TunnelHint

  toProto (THint linf hints) =
    P.TunnelHint
      (toProto linf)
      (toProto hints)
      (fromIntegral (value @e :: Int))
      (fromIntegral (value @r :: Int))
      (fromIntegral (value @s :: Int))
      (fromIntegral $ modulus @zp)

  fromProto (P.TunnelHint linf hints e r s p) =
    let e' = fromIntegral $ (value @e :: Int)
        r' = fromIntegral $ (value @r :: Int)
        s' = fromIntegral $ (value @s :: Int)
        p' = fromIntegral $ modulus @zp
    in if p' == p && e' == e && r' == r && s' == s
       then do
         linf' <- fromProto linf
         hs <- fromProto hints
         return $ THint linf' hs
       else error $ "Error reading TunnelHint proto data:" ++
              "\nexpected p=" ++ show p' ++ ", got " ++ show p ++
              "\nexpected e=" ++ show e' ++ ", got " ++ show e ++
              "\nexpected r=" ++ show r' ++ ", got " ++ show r ++
              "\nexpected s=" ++ show s' ++ ", got " ++ show s
