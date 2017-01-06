{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Symmetric-key somewhat homomorphic encryption.  See Section 4 of
-- http://eprint.iacr.org/2015/1134 for mathematical description.

module Crypto.Lol.Applications.SymmSHE
(
-- * Data types
SK, PT, CT -- don't export constructors!
-- * Keygen, encryption, decryption
, genSK, genSKWithVar
, encrypt
, errorTerm, errorTermUnrestricted, decrypt, decryptUnrestricted
-- * Arithmetic with public values
, addScalar, addPublic, mulScalar, mulPublic
-- * Modulus switching
, rescaleLinearCT, modSwitchPT
-- * Key switching
, KSLinearHint, KSQuadCircHint
, ksLinearHint, ksQuadCircHint
, keySwitchLinear, keySwitchQuadCirc
-- * Ring switching
, embedSK, embedCT, twaceCT
, TunnelInfo, tunnelInfo
, tunnelCT
-- * Constraint synonyms
, GenSKCtx, EncryptCtx, ToSDCtx, ErrorTermCtx
, DecryptCtx, DecryptUCtx
, AddScalarCtx, AddPublicCtx, MulScalarCtx, MulPublicCtx, ModSwitchPTCtx
, KeySwitchCtx, KSHintCtx
, GenTunnelInfoCtx, TunnelCtx
, SwitchCtx, LWECtx -- these are internal, but exported for better docs
) where

import qualified Algebra.Additive as Additive (C)
import qualified Algebra.Ring     as Ring (C)

import Crypto.Lol as LP hiding (sin)
import Crypto.Lol.Cyclotomic.UCyc   (D, UCyc)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto
import Crypto.Proto.Lol.R (R)
import Crypto.Proto.Lol.RqProduct (RqProduct)
import qualified Crypto.Proto.SHE.KSHint as P
import qualified Crypto.Proto.SHE.RqPolynomial as P
import qualified Crypto.Proto.SHE.SecretKey as P
import qualified Crypto.Proto.SHE.TunnelInfo as P

import Control.Applicative  hiding ((*>))
import Control.DeepSeq
import Control.Monad        as CM
import Control.Monad.Random
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
data CT (m :: Factored) zp r'q =
  CT
  !Encoding                     -- MSD/LSD encoding
  !Int                          -- accumulated power of g_m' in c(s)
  !zp                           -- factor to mul by upon decryption
  !(Polynomial r'q)             -- the polynomial c(s)
  deriving (Show)

-- Note: do *not* give an Eq instance for CT, because it's not
-- meaningful to compare ciphertexts for equality

instance (NFData zp, NFData r'q) => NFData (CT m zp r'q) where
  rnf (CT _ k sc cs) = rnf k `seq` rnf sc `seq` rnf cs

instance (NFData r) => NFData (SK r) where
  rnf (SK v s) = rnf v `seq` rnf s

---------- Basic functions: Gen, Enc, Dec ----------

-- | Constraint synonym for generating a secret key.
type GenSKCtx t m z v =
  (ToInteger z, Fact m, CElt t z, ToRational v, NFData v)

-- | Generates a secret key with (index-independent) scaled variance
-- parameter \( v \); see 'errorRounded'.
genSK :: (GenSKCtx t m z v, MonadRandom rnd)
      => v -> rnd (SK (Cyc t m z))
genSK v = liftM (SK v) $ errorRounded v

-- | Generates a secret key with the same scaled variance
-- as the input secret key.
genSKWithVar :: (ToInteger z, Fact m, CElt t z, MonadRandom rnd)
             => SK a -> rnd (SK (Cyc t m z))
genSKWithVar (SK v _) = genSK v

-- | Constraint synonym for encryption.
type EncryptCtx t m m' z zp zq =
  (Mod zp, Ring zp, Ring zq, Lift zp (ModRep zp), Random zq,
   Reduce z zq, Reduce (LiftOf zp) zq,
   CElt t zq, CElt t zp, CElt t z, CElt t (LiftOf zp),
   m `Divides` m')

-- | Encrypt a plaintext under a secret key.
encrypt :: forall t m m' z zp zq rnd .
  (EncryptCtx t m m' z zp zq, MonadRandom rnd)
  => SK (Cyc t m' z) -> PT (Cyc t m zp) -> rnd (CT m zp (Cyc t m' zq))
encrypt (SK svar s) =
  let sq = adviseCRT $ reduce s
  in \pt -> do
    e <- errorCoset svar (embed pt :: PT (Cyc t m' zp))
    c1 <- getRandom
    return $! CT LSD zero one $ fromCoeffs [reduce e - c1 * sq, c1]

-- | Constraint synonym for extracting the error term of a ciphertext.
type ErrorTermCtx t m' z zp zq =
  (Reduce z zq, Lift' zq, CElt t z, CElt t (LiftOf zq), ToSDCtx t m' zp zq)

-- | Extract the error term of a ciphertext.
errorTerm :: (ErrorTermCtx t m' z zp zq)
             => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> Cyc t m' (LiftOf zq)
errorTerm (SK _ s) = let sq = reduce s in
  \ct -> let (CT LSD _ _ c) = toLSD ct
         in liftCyc Dec $ evaluate c sq

-- for when we know the division must succeed
divG' :: (Fact m, CElt t r, IntegralDomain r) => Cyc t m r -> Cyc t m r
divG' = fromJust . divG

-- | Constraint synonym for decryption.
type DecryptCtx t m m' z zp zq =
  (ErrorTermCtx t m' z zp zq, Reduce (LiftOf zq) zp, IntegralDomain zp,
   m `Divides` m', CElt t zp)

-- | Decrypt a ciphertext.
decrypt :: forall t m m' z zp zq . (DecryptCtx t m m' z zp zq)
           => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> PT (Cyc t m zp)
decrypt sk ct =
  let ct'@(CT LSD k l _) = toLSD ct
  in let e :: Cyc t m' zp = reduce $ errorTerm sk ct'
     in (scalarCyc l) * twace (iterate divG' e !! k)

--- unrestricted versions ---
-- | Constraint synonym for unrestricted decryption.
type DecryptUCtx t m m' z zp zq =
  (Fact m, Fact m', CElt t zp, m `Divides` m',
   Reduce z zq, Lift' zq, CElt t z,
   ToSDCtx t m' zp zq, Reduce (LiftOf zq) zp, IntegralDomain zp)

-- | More general form of 'errorTerm' that works for unrestricted
-- output coefficient types.
errorTermUnrestricted ::
  (Reduce z zq, Lift' zq, CElt t z, ToSDCtx t m' zp zq)
  => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> UCyc t m' D (LiftOf zq)
errorTermUnrestricted (SK _ s) = let sq = reduce s in
  \ct -> let (CT LSD _ _ c) = toLSD ct
             eval = evaluate c sq
         in fmap lift $ uncycDec eval

-- | More general form of 'decrypt' that works for unrestricted output
-- coefficient types.
decryptUnrestricted :: (DecryptUCtx t m m' z zp zq)
  => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> PT (Cyc t m zp)
decryptUnrestricted (SK _ s) = let sq = reduce s in
  \ct -> let (CT LSD k l c) = toLSD ct
         in let eval = evaluate c sq
                e = cycDec $ fmap (reduce . lift) $ uncycDec eval
                l' = scalarCyc l
            in l' * twace (iterate divG' e !! k)

---------- LSD/MSD switching ----------

-- | Constraint synonym for converting between ciphertext encodings.
type ToSDCtx t m' zp zq = (Encode zp zq, Fact m', CElt t zq)

toLSD, toMSD :: ToSDCtx t m' zp zq
 => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)

-- CJP: reduce duplication in these functions?  They differ in only two places

-- | Convert a ciphertext to MSD encoding.
toMSD = let (zpScale, zqScale) = lsdToMSD
            rqScale = scalarCyc zqScale
        in \ct@(CT enc k l c) -> case enc of
          MSD -> ct
          LSD -> CT MSD k (zpScale * l) ((rqScale *) <$> c)

-- | Convert a ciphertext to LSD encoding.
toLSD = let (zpScale, zqScale) = msdToLSD
            rqScale = scalarCyc zqScale
        in \ct@(CT enc k l c) -> case enc of
          LSD -> ct
          MSD -> CT LSD k (zpScale * l) ((rqScale *) <$> c)

---------- Modulus switching ----------

-- | Rescale a linear polynomial in MSD encoding, for best noise behavior.
rescaleLinearMSD :: (RescaleCyc (Cyc t) zq zq', Fact m')
                    => Polynomial (Cyc t m' zq) -> Polynomial (Cyc t m' zq')
rescaleLinearMSD c = case coeffs c of
  [] -> fromCoeffs []
  [c0] -> fromCoeffs [rescaleDec c0]
  [c0,c1] -> let c0' = rescaleDec c0
                 c1' = rescalePow c1
             in fromCoeffs [c0', c1']
  _ -> error $ "rescaleLinearMSD: list too long (not linear): " ++
       show (length $ coeffs c)

-- | Rescale a linear ciphertext to a new modulus.
rescaleLinearCT :: (RescaleCyc (Cyc t) zq zq', ToSDCtx t m' zp zq)
           => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq')
rescaleLinearCT ct = let CT MSD k l c = toMSD ct
                     in CT MSD k l $ rescaleLinearMSD c

-- | Constraint synonym for modulus switching.
type ModSwitchPTCtx t m' zp zp' zq =
  (Lift' zp, Reduce (LiftOf zp) zp', ToSDCtx t m' zp zq)

-- | Homomorphically divide a plaintext that is known to be a multiple
-- of \( (p/p') \) by that factor, thereby scaling the plaintext modulus
-- from \( p \) to \( p' \).
modSwitchPT :: (ModSwitchPTCtx t m' zp zp' zq)
            => CT m zp (Cyc t m' zq) -> CT m zp' (Cyc t m' zq)
modSwitchPT ct = let CT MSD k l c = toMSD ct in
    CT MSD k (reduce (lift l)) c

---------- Key switching ----------

-- | Constraint synonym for generating an LWE sample.
type LWECtx t m' z zq =
  (ToInteger z, Reduce z zq, Ring zq, Random zq, Fact m', CElt t z, CElt t zq)

-- An LWE sample for a given secret (corresponding to a linear
-- ciphertext encrypting 0 in MSD form)
lweSample :: (LWECtx t m' z zq, MonadRandom rnd)
             => SK (Cyc t m' z) -> rnd (Polynomial (Cyc t m' zq))
lweSample (SK svar s) =
  -- adviseCRT because we call `replicateM (lweSample s)` below, but only want to do CRT once.
  let sq = adviseCRT $ negate $ reduce s
  in do
    e <- errorRounded svar
    c1 <- adviseCRT <$> getRandom -- want entire hint to be in CRT form
    return $ fromCoeffs [c1 * sq + reduce (e `asTypeOf` s), c1]

-- | Constraint synonym for generating key-switch hints.
type KSHintCtx gad t m' z zq =
  (LWECtx t m' z zq, Reduce (DecompOf zq) zq, Gadget gad zq,
   NFElt zq, CElt t (DecompOf zq))

-- | Generate a hint that "encrypts" a value under a secret key, in
-- the sense required for key-switching.  The hint works for any
-- plaintext modulus, but must be applied on a ciphertext in MSD form.
-- The output is 'force'd, i.e., evaluating it to whnf will actually
-- cause it to be be evaluated to nf.
ksHint :: (KSHintCtx gad t m' z zq, MonadRandom rnd)
          => SK (Cyc t m' z) -> Cyc t m' z
          -> rnd (Tagged gad [Polynomial (Cyc t m' zq)])
ksHint skout val = do -- rnd monad
  let valq = reduce val
      valgad = encode valq
  -- CJP: clunky, but that's what we get without a MonadTagged
  samples <- DT.mapM (\as -> replicateM (length as) (lweSample skout)) valgad
  return $! force $ zipWith (+) <$> (map P.const <$> valgad) <*> samples

-- poor man's module multiplication for knapsack
(*>>) :: (Ring r, Functor f) => r -> f r -> f r
(*>>) r = fmap (r *)

knapsack :: (Fact m', CElt t zq, r'q ~ Cyc t m' zq)
            => [Polynomial r'q] -> [r'q] -> Polynomial r'q
-- adviseCRT here because we map (x *) onto each polynomial coeff
knapsack hint xs = sum $ zipWith (*>>) (adviseCRT <$> xs) hint

-- | Constraint synonym for applying a key-switch hint.
type SwitchCtx gad t m' zq =
  (Decompose gad zq, Fact m', CElt t zq, CElt t (DecompOf zq))

-- Helper function: applies key-switch hint to a ring element.
switch :: (SwitchCtx gad t m' zq, r'q ~ Cyc t m' zq)
          => Tagged gad [Polynomial r'q] -> r'q -> Polynomial r'q
switch hint c = untag $ knapsack <$> hint <*> (fmap reduce <$> decompose c)

-- | Constraint synonym for key switching.
type KeySwitchCtx gad t m' zp zq zq' =
  (RescaleCyc (Cyc t) zq' zq, RescaleCyc (Cyc t) zq zq',
   ToSDCtx t m' zp zq, SwitchCtx gad t m' zq')

-- | Hint for a linear key switch
newtype KSLinearHint gad r'q' = KSLHint (Tagged gad [Polynomial r'q']) deriving (NFData)

-- | Hint for a circular quadratic key switch.
newtype KSQuadCircHint gad r'q' = KSQHint (Tagged gad [Polynomial r'q']) deriving (NFData)

-- | A hint to switch a linear ciphertext under \( s_{\text{in}} \) to a linear
-- one under \( s_{\text{out}} \).
ksLinearHint :: (KSHintCtx gad t m' z zq', MonadRandom rnd)
  => SK (Cyc t m' z) -- sout
  -> SK (Cyc t m' z) -- sin
  -> rnd (KSLinearHint gad (Cyc t m' zq'))
ksLinearHint skout (SK _ sin) = KSLHint <$> ksHint skout sin

-- | Switch a linear ciphertext using the supplied hint.
keySwitchLinear :: (KeySwitchCtx gad t m' zp zq zq')
  => KSLinearHint gad (Cyc t m' zq') -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)
keySwitchLinear (KSLHint hint) ct =
  let CT MSD k l c = toMSD ct
      [c0,c1] = coeffs c
      c1' = rescalePow c1
  in CT MSD k l $ P.const c0 + rescaleLinearMSD (switch hint c1')

-- | A hint to switch a quadratic ciphertext to a linear
-- one under the same key.
ksQuadCircHint :: (KSHintCtx gad t m' z zq', MonadRandom rnd)
  => SK (Cyc t m' z)
  -> rnd (KSQuadCircHint gad (Cyc t m' zq'))
ksQuadCircHint sk@(SK _ s) = KSQHint <$> ksHint sk (s*s)

-- | Switch a quadratic ciphertext (i.e., one with three components)
-- to a linear one under the /same/ key using the supplied hint.
keySwitchQuadCirc :: (KeySwitchCtx gad t m' zp zq zq')
  => KSQuadCircHint gad (Cyc t m' zq') -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)
keySwitchQuadCirc (KSQHint hint) ct =
  let CT MSD k l c = toMSD ct
      [c0,c1,c2] = coeffs c
      c2' = rescalePow c2
  in CT MSD k l $ P.fromCoeffs [c0,c1] + rescaleLinearMSD (switch hint c2')

---------- Misc homomorphic operations ----------
-- | Constraint synonym for adding a public scalar to a ciphertext.
type AddScalarCtx t m' zp zq =
  (Lift' zp, Reduce (LiftOf zp) zq,
   CElt t zp, CElt t (LiftOf zp), ToSDCtx t m' zp zq)

-- | Homomorphically add a public \(\mathbb{Z}_p\) value to an encrypted value.
addScalar :: forall t m m' zp zq . (AddScalarCtx t m' zp zq)
          => zp -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)
addScalar b ct =
  let CT LSD k l c = toLSD ct
      b' = iterate mulG (scalarCyc $ b * recip l) !! k :: Cyc t m' zp
  in CT LSD k l $ c + (P.const $ reduce $ liftPow b')

-- | Constraint synonym for adding a public value to an encrypted value.
type AddPublicCtx t m m' zp zq = (AddScalarCtx t m' zp zq, m `Divides` m')

-- | Homomorphically add a public \( R_p \) value to an encrypted
-- value.
addPublic :: forall t m m' zp zq . (AddPublicCtx t m m' zp zq)
          => Cyc t m zp -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)
addPublic b ct = let CT LSD k l c = toLSD ct in
  let linv = scalarCyc $ recip l
      -- multiply public value by appropriate power of g and divide by the
      -- scale, to match the form of the ciphertext
      b' :: Cyc t m zq = reduce $ liftPow $ linv * (iterate mulG b !! k)
  in CT LSD k l $ c + P.const (embed b')

-- | Constraint synonym for multiplying a public scalar value with
-- an encrypted value.
type MulScalarCtx t m' zp zq =
  (Lift' zp, Reduce (LiftOf zp) zq, Fact m', CElt t zq)

-- | Homomorphically multiply a public \(\mathbb{Z}_p\) value to an
-- encrypted value.
mulScalar :: (MulScalarCtx t m' zp zq)
  => zp -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)
mulScalar a (CT enc k l c) =
  let a' = scalarCyc $ reduce $ lift a
  in CT enc k l $ (a' *) <$> c

-- | Constraint synonym for multiplying a public value with an encrypted value.
type MulPublicCtx t m m' zp zq =
  (MulScalarCtx t m' zp zq, m `Divides` m', CElt t zp, CElt t (LiftOf zp))

-- | Homomorphically multiply an encrypted value by a public \( R_p \)
-- value.
mulPublic :: forall t m m' zp zq . (MulPublicCtx t m m' zp zq)
             => Cyc t m zp -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)
mulPublic a (CT enc k l c) =
  let a' = embed (reduce $ liftPow a :: Cyc t m zq)
  in CT enc k l $ (a' *) <$> c

-- | Increment the internal \( g \) exponent without changing the
-- encrypted message.
mulGCT :: (Fact m', CElt t zq)
          => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)
mulGCT (CT enc k l c) = CT enc (k+1) l $ mulG <$> c

---------- NumericPrelude instances ----------

instance (Eq zp, MulScalarCtx t m' zp zq, m `Divides` m', ToSDCtx t m' zp zq)
         => Additive.C (CT m zp (Cyc t m' zq)) where

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

instance (ToSDCtx t m' zp zq, Additive (CT m zp (Cyc t m' zq)))
  => Ring.C (CT m zp (Cyc t m' zq)) where

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

type AbsorbGCtx t m' zp zq =
  (Lift' zp, IntegralDomain zp, Reduce (LiftOf zp) zq, Ring zq,
   Fact m', CElt t (LiftOf zp), CElt t zp, CElt t zq)

-- | "Absorb" the powers of \( g \) associated with the ciphertext, at
-- the cost of some increase in noise. This is usually needed before
-- changing the index of the ciphertext ring.
absorbGFactors :: forall t zp zq m m' . (AbsorbGCtx t m' zp zq)
                  => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)
absorbGFactors ct@(CT enc k l c)
  | k == 0 = ct
  | k > 0 = let d :: Cyc t m' zp = iterate divG' one !! k
                rep = adviseCRT $ reduce $ liftPow d
            in CT enc 0 l $ (rep *) <$> c
  | otherwise = error "k < 0 in absorbGFactors"

-- | Embed a ciphertext in \( R' \) encrypting a plaintext in \( R \) to
-- a ciphertext in \( T' \) encrypting a plaintext in \( T \). The target
-- ciphertext ring \( T' \) must contain both the the source ciphertext
-- ring \( R' \) and the target plaintext ring \( T \).
embedCT :: (CElt t zq,
            r `Divides` r', s `Divides` s', r `Divides` s, r' `Divides` s')
           => CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' zq)
-- We could call absorbGFactors first, insead of error.  Embedding
-- *essentially* maintains the invariant that noise*g is "round."
-- While g'/g can be non-spherical, it only stretches by at most a
-- factor of 2 per new odd prime.  We *cannot* multiply by g, then
-- embed, then divide by g' because the result would not remain in R'.
-- So this is the best we can do.
embedCT (CT d 0 l c) = CT d 0 l (embed <$> c)
embedCT _ = error "embedCT requires 0 factors of g; call aborbGFactors first"

-- | Embed a secret key from a subring into a superring.
embedSK :: (m `Divides` m') => SK (Cyc t m z) -> SK (Cyc t m' z)
embedSK (SK v s) = SK v $ embed s

-- | "Tweaked trace" function for ciphertexts.  Mathematically, the
-- target plaintext ring \( S \) must contain the intersection of the
-- source plaintext ring \( T \) and the target ciphertext ring \( S'
-- \).  Here we make the stricter requirement that \( s = \gcd(s', t)
-- \).
twaceCT :: (CElt t zq, r `Divides` r', s' `Divides` r',
            s ~ (FGCD s' r))
           => CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' zq)
-- we could call absorbGFactors first, insead of error
twaceCT (CT d 0 l c) = CT d 0 l (twace <$> c)
twaceCT _ = error "twaceCT requires 0 factors of g; call absorbGFactors first"


data TunnelInfo gad t (e :: Factored) (r :: Factored) (s :: Factored) e' r' s' zp zq =
  TInfo (Linear t zq e' r' s') [Tagged gad [Polynomial (Cyc t s' zq)]]

instance (NFData (Linear t zq e' r' s'), NFData (Cyc t s' zq))
  => NFData (TunnelInfo gad t e r s e' r' s' zp zq) where
  rnf (TInfo l t) = rnf l `seq` rnf t

-- EAC: `e' ~ (e * ...) is not needed in this module, but it is needed as use sites...
type GenTunnelInfoCtx t e r s e' r' s' z zp zq gad =
  (ExtendLinIdx e r s e' r' s', -- extendLin
   e' ~ (e * (r' / r)),         -- convenience; implied by prev constraint
   KSHintCtx gad t r' z zq,     -- ksHint
   Lift zp z, CElt t zp,        -- liftLin
   CElt t z, e' `Divides` r')   -- powBasis

tunnelInfo :: forall gad t e r s e' r' s' z zp zq rnd .
  (MonadRandom rnd, GenTunnelInfoCtx t e r s e' r' s' z zp zq gad)
  => Linear t zp e r s
  -> SK (Cyc t s' z)
  -> SK (Cyc t r' z)
  -> rnd (TunnelInfo gad t e r s e' r' s' zp zq)
tunnelInfo f skout (SK _ sin) = -- generate hints
  (let f' = extendLin $ lift f :: Linear t z e' r' s'
       f'q = reduce f' :: Linear t zq e' r' s'
       -- choice of basis here must match coeffs* basis below
       ps = proxy powBasis (Proxy::Proxy e')
       comps = (evalLin f' . (adviseCRT sin *)) <$> ps
   in TInfo f'q <$> CM.mapM (ksHint skout) comps)
    \\ lcmDivides (Proxy::Proxy r) (Proxy::Proxy e')

-- | Constraint synonym for ring tunneling.
type TunnelCtx t r s e' r' s' zp zq gad =
  (Fact r, Fact s, e' `Divides` r', e' `Divides` s', CElt t zp, -- evalLin
   ToSDCtx t r' zp zq,                                          -- toMSD
   AbsorbGCtx t r' zp zq,                                       -- absorbGFactors
   SwitchCtx gad t s' zq)                                       -- switch

-- | Homomorphically apply the \( E \)-linear function that maps the
-- elements of the decoding basis of \( R/E \) to the corresponding
-- \( S \)-elements in the input array.
tunnelCT :: forall gad t e r s e' r' s' zp zq .
  (TunnelCtx t r s e' r' s' zp zq gad, e ~ FGCD r s)
  => TunnelInfo gad t e r s e' r' s' zp zq
  -> CT r zp (Cyc t r' zq)
  -> CT s zp (Cyc t s' zq)
tunnelCT (TInfo f'q hints) ct =
  (let CT MSD 0 s c = toMSD $ absorbGFactors ct
       [c0,c1] = coeffs c
       -- apply E-linear function to constant term c0
       c0' = evalLin f'q c0
       -- apply E-linear function to c1 via key-switching
       -- this basis must match the basis used above to generate the hints
       c1s = coeffsPow c1 :: [Cyc t e' zq]
       -- CJP: don't embed the c1s before decomposing them (inside
       -- switch); instead decompose in smaller ring before
       -- embedding (it matters).
       -- We may need to generalize switch or define an
       -- alternative.
       c1s' = zipWith switch hints (embed <$> c1s)
       c1' = sum c1s'
   in CT MSD 0 s $ P.const c0' + c1')
    \\ lcmDivides (Proxy::Proxy r) (Proxy::Proxy e')

instance (Protoable r, ProtoType r ~ R) => Protoable (SK r) where
  type ProtoType (SK r) = P.SecretKey
  toProto (SK v r) = P.SecretKey (toProto r) (realToField v)
  fromProto (P.SecretKey r v) = (SK v) <$> fromProto r

instance (Protoable rq, ProtoType rq ~ RqProduct) => Protoable (Polynomial rq) where
  type ProtoType (Polynomial rq) = P.RqPolynomial
  toProto = P.RqPolynomial . toProto . coeffs
  fromProto (P.RqPolynomial x) = fromCoeffs <$> fromProto x

instance (Typeable gad, Protoable r'q', ProtoType r'q' ~ RqProduct)
  => Protoable (KSLinearHint gad r'q') where
  type ProtoType (KSLinearHint gad r'q') = P.KSHint
  toProto (KSLHint cs) =
    P.KSHint
      (toProto $ proxy cs (Proxy::Proxy gad))
      (toProto $ typeRepFingerprint $ typeRep (Proxy::Proxy gad))
  fromProto (P.KSHint poly gadrepr') = do
    let gadrepr = toProto $ typeRepFingerprint $ typeRep (Proxy::Proxy gad)
    if gadrepr == gadrepr'
    then (KSLHint . tag) <$> fromProto poly
    else error $ "Expected gadget " ++ (show $ typeRep (Proxy::Proxy gad))

instance (Typeable gad, Protoable r'q', ProtoType r'q' ~ RqProduct)
  => Protoable (KSQuadCircHint gad r'q') where
  type ProtoType (KSQuadCircHint gad r'q') = P.KSHint
  toProto (KSQHint x) = toProto $ KSLHint x
  fromProto y = do
    (KSLHint x) <- fromProto y
    return $ KSQHint x

instance (Mod zp, Typeable gad,
          Protoable (Linear t zq e' r' s'),
          Protoable (KSLinearHint gad (Cyc t s' zq)), Reflects s Int, Reflects r Int, Reflects e Int)
  => Protoable (TunnelInfo gad t e r s e' r' s' zp zq) where
  type ProtoType (TunnelInfo gad t e r s e' r' s' zp zq) = P.TunnelInfo
  toProto (TInfo linf hints) =
    P.TunnelInfo
      (toProto linf)
      (toProto $ KSLHint <$> hints)
      (fromIntegral (proxy value (Proxy::Proxy e) :: Int))
      (fromIntegral (proxy value (Proxy::Proxy r) :: Int))
      (fromIntegral (proxy value (Proxy::Proxy s) :: Int))
      (fromIntegral $ proxy modulus (Proxy::Proxy zp))
  fromProto (P.TunnelInfo linf hints e r s p) =
    let e' = fromIntegral $ (proxy value (Proxy::Proxy e) :: Int)
        r' = fromIntegral $ (proxy value (Proxy::Proxy r) :: Int)
        s' = fromIntegral $ (proxy value (Proxy::Proxy s) :: Int)
        p' = fromIntegral $ proxy modulus (Proxy::Proxy zp)
    in if p' == p && e' == e && r' == r && s' == s
       then do
         linf' <- fromProto linf
         hs <- (map (\(KSLHint x) -> x)) <$> fromProto hints
         return $ TInfo linf' hs
       else error $ "Error reading TunnelInfo proto data:" ++
              "\nexpected p=" ++ show p' ++ ", got " ++ show p ++
              "\nexpected e=" ++ show e' ++ ", got " ++ show e ++
              "\nexpected r=" ++ show r' ++ ", got " ++ show r ++
              "\nexpected s=" ++ show s' ++ ", got " ++ show s
