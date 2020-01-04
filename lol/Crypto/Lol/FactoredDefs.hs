{-|
Module      : Crypto.Lol.FactoredDefs
Description : Internal module.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

This sub-module exists only because we can't define and use
template Haskell splices in the same module.
-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NoStarIsType          #-}

{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Crypto.Lol.FactoredDefs
(
-- * Factored natural numbers
  Factored, SFactored, Fact, fType, fDec
, reifyFact, reifyFactI, intToFact
-- * Prime powers
, PrimePower(..), SPrimePower(SPP), PPow, ppType, ppDec
, reifyPPow, reifyPPowI
-- * Primes
, PrimeBin, SPrimeBin, Prime, pType, pDec
, reifyPrime, reifyPrimeI, valueP
-- * Constructors
, pToPP, sPToPP, PToPP, ppToF, sPpToF, PpToF, pToF, sPToF, PToF
-- * Unwrappers
, unF, sUnF, UnF, unPP, sUnPP, UnPP, primePP, PrimePP, exponentPP, ExponentPP
-- * Arithmetic operations
, fPPMul, FPPMul, fMul, FMul, type (*)
, fDivides, FDivides, Divides, fDiv, FDiv, type (/)
, fGCD, FGCD, fLCM, FLCM, Coprime
, fOddRadical, FOddRadical
, pFree, PFree
-- * Entailments
, ForallFact1(..), ForallFact2(..)
-- * Reflections
, ppsFact, valueFact, totientFact, radicalFact, oddRadicalFact, valueHatFact
, ppPPow, primePPow, exponentPPow, valuePPow, totientPPow, radicalPPow, oddRadicalPPow, valueHatPPow
, valuePrime
-- * Operations on 'Factored' values
, valueF, totientF, radicalF, oddRadicalF, valueHatF
-- CJP: are the above actually used anywhere? Don't see that they're needed
-- * Number-theoretic laws
, transDivides, gcdDivides, lcmDivides, lcm2Divides
, pSplitTheorems, pFreeDivides
, (\\) -- re-export from Data.Constraint for convenience
-- * Utility operations on prime powers
, valueHat
, PP, ppToPP, valuePP, totientPP, radicalPP, oddRadicalPP, valueHatPP
-- * Re-export
, module Crypto.Lol.PosBin
) where

import Crypto.Lol.PosBin

import Control.Arrow
import Data.Constraint           hiding ((***), (&&&))
import Data.List                 hiding ((\\))
import Data.Singletons.Prelude   hiding ((:-), type (*))
import Data.Singletons.TH
import Language.Haskell.TH

import Unsafe.Coerce

singletons [d|

            -- CJP: record syntax doesn't work here with singletons;
            -- something about "escaped type variables"

            -- restrict to primes
            newtype PrimeBin = P Bin deriving (Eq,Ord,Show)

            -- (prime, exponent)
            newtype PrimePower = PP (PrimeBin,Pos) deriving (Eq,Show)

            -- Invariant: primes appear in strictly increasing
            -- order (no duplicates).
            newtype Factored = F [PrimePower] deriving (Eq,Show)

            -- Unwrap 'PrimeBin'
            unP :: PrimeBin -> Bin
            unP (P p) = p

            -- Unwrap 'PrimePower'.
            unPP :: PrimePower -> (PrimeBin,Pos)
            unPP (PP pp) = pp

            -- Unwrap 'Factored'
            unF :: Factored -> [PrimePower]
            unF (F pps) = pps

            -- Prime component of a 'PrimePower'.
            primePP :: PrimePower -> PrimeBin
            primePP = fst . unPP

            -- Exponent component of a 'PrimePower'.
            exponentPP :: PrimePower -> Pos
            exponentPP = snd . unPP

            |]

type F1 = 'F '[]

singletons [d|

            fPPMul :: PrimePower -> Factored -> Factored
            fMul :: Factored -> Factored -> Factored

            -- Multiply a 'PrimePower' into a 'Factored' number.
            fPPMul pp (F pps) = F (ppMul pp pps)

            -- Multiply two 'Factored' numbers.
            fMul (F pps1) (F pps2) = F (ppsMul pps1 pps2)

            -- helper functions (not for export)

            -- keeps primes in sorted order; merges duplicates
            ppMul :: PrimePower -> [PrimePower] -> [PrimePower]
            ppMul x [] = [x]
            ppMul pp'@(PP (p',e')) pps@(pp@(PP (p,e)):pps') =
              case compare p' p of
                EQ -> PP (p, addPos e e') : pps'
                LT -> pp' : pps
                GT -> pp : ppMul pp' pps'

            ppsMul :: [PrimePower] -> [PrimePower] -> [PrimePower]
            ppsMul [] ys = ys
            ppsMul (pp:pps) ys = ppsMul pps (ppMul pp ys)

            |]

-- ARITHMETIC OPERATIONS
singletons [d|
            pToPP :: PrimeBin -> PrimePower
            pToPP p = PP (p, O)

            ppToF :: PrimePower -> Factored
            ppToF pp = F [pp]

            pToF :: PrimeBin -> Factored
            pToF = ppToF . pToPP

            fGCD, fLCM :: Factored -> Factored -> Factored
            fDivides :: Factored -> Factored -> Bool
            fDiv :: Factored -> Factored -> Factored
            fOddRadical :: Factored -> Factored

            fGCD (F pps1) (F pps2) = F (ppsGCD pps1 pps2)
            fLCM (F pps1) (F pps2) = F (ppsLCM pps1 pps2)

            fDivides (F pps1) (F pps2) = ppsDivides pps1 pps2
            fDiv (F pps1) (F pps2) = F (ppsDiv pps1 pps2)
            fOddRadical (F pps) = F (ppsOddRad pps)

            -- Helper functions (not for export) on PrimePowers and
            -- lists.  Can assume that input lists obey the invariant
            -- of Factored lists, and need to ensure that output lists
            -- also obey the invariant.
            ppsGCD :: [PrimePower] -> [PrimePower] -> [PrimePower]
            ppsGCD [] _ = []
            ppsGCD (_:_) [] = []
            ppsGCD xs@(PP (p,e) : xs') ys@(PP (p',e') : ys') =
              case compare p p' of
                EQ -> PP (p, min e e') : ppsGCD xs' ys'
                LT -> ppsGCD xs' ys
                GT -> ppsGCD xs  ys'

            ppsLCM :: [PrimePower] -> [PrimePower] -> [PrimePower]
            ppsLCM [] ys = ys
            ppsLCM xs@(_:_) [] = xs
            ppsLCM xs@(pp@(PP (p,e)) : xs') ys@(pp'@(PP (p',e')) : ys') =
              case compare p p' of
                EQ -> PP (p, max e e') : ppsLCM xs' ys'
                LT -> pp  : ppsLCM xs' ys
                GT -> pp' : ppsLCM xs  ys'

            ppsDivides :: [PrimePower] -> [PrimePower] -> Bool
            ppsDivides [] _ = True
            ppsDivides (_:_) [] = False
            ppsDivides xs@(PP (p,e) : xs') (PP (p',e') : ys') =
              if p == p' then (e <= e') && ppsDivides xs' ys'
              else (p > p') && ppsDivides xs ys'

            ppsDiv :: [PrimePower] -> [PrimePower] -> [PrimePower]
            ppsDiv xs [] = xs
            ppsDiv (pp@(PP (p,e)) : xs') ys@(PP (p',e') : ys') =
              if p == p' && e' == e then ppsDiv xs' ys'
              else if p == p' && e' <= e then PP (p, subPos e e') : ppsDiv xs' ys'
              else if p <= p' then pp : ppsDiv xs' ys
              else error "invalid call to ppsDiv"

            ppsOddRad :: [PrimePower] -> [PrimePower]
            ppsOddRad [] = []
            ppsOddRad (PP (p, _) : xs') =
                if p == P (D0 B1) then ppsOddRad xs' -- D0 B1 == 2
                else PP (p,O) : ppsOddRad xs'

            |]

singletons [d|
            -- Remove all @p@-factors from a 'Factored'.
            pFree :: PrimeBin -> Factored -> Factored
            pFree p (F pps) = F (go pps)
              where go [] = []
                    go (pp@(PP (p',_)) : ps) =
                      if p == p' then ps
                      else pp : (go ps)
            |]

-- | Contraint 'c' holds for 't m' for any 'Fact m'.
class ForallFact1 c t where
  entailFact1 :: (Fact m) :- (c (t m))

-- | Contraint 'c' holds for 't m r' for any 'Fact m'.
class ForallFact2 c t r where
  entailFact2 :: (Fact m) :- (c (t m r))

-- | Type (family) synonym for division of 'Factored' types.
type a / b = FDiv a b

-- | Type (family) synonym for multiplication of 'Factored' types.
type a * b = FMul a b

-- convenience aliases: enforce kind, hide SingI

-- | Kind-restricted synonym for 'SingI'.
type Prime (p :: PrimeBin) = SingI p

-- | Kind-restricted synonym for 'SingI'.
type PPow (pp :: PrimePower) = SingI pp

-- | Kind-restricted synonym for 'SingI'.
type Fact (m :: Factored) = SingI m

-- | Reify a 'PrimeBin' as a singleton.
reifyPrime :: Int -> (forall p . SPrimeBin p -> a) -> a
reifyPrime x _ | not $ prime x = error $ "reifyPrime: non-prime x = " ++ show x
reifyPrime x k = reifyBin x (k . SP)

-- | Reify a 'PrimeBin' for a 'Prime' constraint.
reifyPrimeI :: Int -> (forall p proxy. Prime p => proxy p -> a) -> a
reifyPrimeI n k =
  reifyPrime n (\(p::SPrimeBin p) -> withSingI p $ k (Proxy::Proxy p))

-- | Reify a 'PrimePower' as a singleton.
reifyPPow :: (Int,Int) -> (forall pp . SPrimePower pp -> a) -> a
reifyPPow (p,e) k = reifyPrime p (\sp -> reifyPos e (k . SPP . STuple2 sp))

-- | Reify a 'PrimePower' for a 'PPow' constraint.
reifyPPowI :: (Int,Int) -> (forall pp proxy. PPow pp => proxy pp -> a) -> a
reifyPPowI pp k =
  reifyPPow pp $ (\(p::SPrimePower p) -> withSingI p $ k (Proxy::Proxy p))

-- | Reify a 'Factored' as a singleton.
reifyFact :: Int -> (forall m . SFactored m -> a) -> a
reifyFact m k = let pps = factorize m in reifyPPs pps $ k . SF
  where reifyPPs :: [(Int,Int)]
                    -> (forall (pps :: [PrimePower]) . Sing pps -> a) -> a
        reifyPPs [] c = c SNil
        reifyPPs (pp:pps) c =
          reifyPPow pp (\spp -> reifyPPs pps (\sm' -> c $ SCons spp sm'))

-- | Reify a 'Factored' for a 'Fact' constraint.
reifyFactI :: Int -> (forall m proxy. Fact m => proxy m -> a) -> a
reifyFactI pps k =
  reifyFact pps $ (\(m::SFactored m) -> withSingI m $ k (Proxy::Proxy m))

-- | Constraint synonym for divisibility of 'Factored' types.
type Divides m m' = (Fact m, Fact m', FDivides m m' ~ 'True)

-- | Constraint synonym for coprimality of 'Factored' types.
type Coprime m m' = (FGCD m m' ~ F1)

-- coercions: using proxy arguments here due to compiler bugs in usage

-- coerce any divisibility relationship we want
coerceFDivs :: forall m m' . (() :- (FDivides m m' ~ 'True))
coerceFDivs = Sub $ unsafeCoerce (Dict :: Dict ())

-- coerce any GCD we want
coerceGCD :: (() :- (FGCD a a' ~ a''))
coerceGCD = Sub $ unsafeCoerce (Dict :: Dict ())

-- | Entails constraint for transitivity of division, i.e.
-- if \( k \mid l \) and \( l \mid m \), then \( k \mid m \).
transDivides :: forall k l m .
                ((k `Divides` l, l `Divides` m) :- (k `Divides` m))
transDivides = Sub Dict \\ coerceFDivs @k @m

-- | Entailment for divisibility by GCD:
-- if \( g=\gcd(m_1,m_2) \) then \( g \mid m_1 \) and \( g \mid m_2 \).
gcdDivides :: forall m1 m2 g .
              ((Fact m1, Fact m2, g ~ FGCD m1 m2) :-
               (g `Divides` m1, g `Divides` m2))
gcdDivides =
  Sub $ withSingI (sFGCD (sing :: SFactored m1) (sing :: SFactored m2))
  Dict \\ coerceFDivs @g @m1
       \\ coerceFDivs @g @m2

-- | Entailment for LCM divisibility:
-- if \( l=\lcm(m_1,m_2) \) then \( m_1 \mid l \) and \( m_2 \mid l \).
lcmDivides :: forall m1 m2 l .
              ((Fact m1, Fact m2, l ~ FLCM m1 m2) :-
               (m1 `Divides` l, m2 `Divides` l))
lcmDivides =
  Sub $ withSingI (sFLCM (sing :: SFactored m1) (sing :: SFactored m2))
  Dict \\ coerceFDivs @m1 @l
       \\ coerceFDivs @m2 @l

-- | Entailment for LCM divisibility:
-- the LCM of two divisors of \( m \) also divides \( m \).
lcm2Divides :: forall m1 m2 m l .
               ((m1 `Divides` m, m2 `Divides` m, l ~ FLCM m1 m2) :-
                (m1 `Divides` l, m2 `Divides` l, (FLCM m1 m2) `Divides` m))
lcm2Divides =
  Sub $ withSingI (sFLCM (sing :: SFactored m1) (sing :: SFactored m2))
  Dict \\ coerceFDivs @(FLCM m1 m2) @m \\ lcmDivides @m1 @m2

-- | Entailment for \( p \)-free division:
-- if \( f \) is \(m \) after removing all \( p \)-factors, then \( f \mid m \)
-- and \( \gcd(f,p)=1 \).
pSplitTheorems :: forall p m f .
                  ((Prime p, Fact m, f ~ PFree p m) :-
                   (f `Divides` m, Coprime (PToF p) f))
pSplitTheorems =
  Sub $ withSingI (sPFree (sing :: SPrimeBin p) (sing :: SFactored m))
  Dict \\ coerceFDivs @f @m
  \\ coerceGCD @(PToF p) @f @F1

-- | Entailment for \( p \)-free division:
-- if \( m \mid m' \), then \( \text{p-free}(m) \mid \text{p-free}(m') \).
pFreeDivides :: forall p m m' .
                ((Prime p, m `Divides` m') :-
                 ((PFree p m) `Divides` (PFree p m')))
pFreeDivides =
  Sub $ withSingI (sPFree (sing :: SPrimeBin p) (sing :: SFactored m)) $
        withSingI (sPFree (sing :: SPrimeBin p) (sing :: SFactored m')) $
        Dict \\ coerceFDivs @(PFree p m) @(PFree p m')

-- | Type synonym for @(prime, exponent)@ pair.
type PP = (Int, Int)

-- | Conversion.
ppToPP :: PrimePower -> PP
ppToPP = (binToInt . unP *** posToInt) . unPP

-- | Reflect a 'PrimePower' type to a 'PP' value.
ppPPow :: forall pp . PPow pp => PP
ppPPow = ppToPP $ fromSing (sing :: SPrimePower pp)

-- | Value-level prime-power factorization tagged by a 'Factored' type.
ppsFact :: forall m . Fact m => [PP]
ppsFact = map ppToPP $ unF $ fromSing (sing :: SFactored m)

-- | The value of a 'PrimeBin' type.
valuePrime :: forall p . Prime p => Int
valuePrime = binToInt $ unP $ fromSing (sing :: SPrimeBin p)


valueFact, totientFact, radicalFact, oddRadicalFact, valueHatFact ::
  forall m . Fact m => Int
-- | The value of a 'Factored' type.
valueFact = valuePPs $ ppsFact @m
-- | The totient of a 'Factored' type's value.
totientFact = totientPPs $ ppsFact @m
-- | The "hat" of a 'Factored' type's value:
-- \( \hat{m} = \begin{cases} m & \mbox{if } m \text{ is odd} \\ m/2 & \text{otherwise} \end{cases} \).
valueHatFact = valueHat $ valueFact @m
-- | The radical (product of prime divisors) of a 'Factored' type.
radicalFact = radicalPPs $ ppsFact @m
-- | The odd radical (product of odd prime divisors) of a 'Factored' type.
oddRadicalFact = oddRadicalPPs $ ppsFact @m

valueF, totientF, radicalF, oddRadicalF, valueHatF :: Factored -> Int
-- | The value of a 'Factored'.
valueF = valuePPs . map ppToPP . unF
-- | Totient of a 'Factored'.
totientF = totientPPs . map ppToPP . unF
-- | The hat of a 'Factored'.
valueHatF = valueHat . valueF
-- | The radical of a 'Factored'.
radicalF = radicalPPs . map ppToPP . unF
-- | The odd radical of a 'Factored'.
oddRadicalF = oddRadicalPPs . map ppToPP . unF

primePPow, exponentPPow, valuePPow, totientPPow, radicalPPow, oddRadicalPPow, valueHatPPow ::
  forall pp . PPow pp => Int
-- | Reflect the prime component of a 'PrimePower' type.
primePPow = fst $ ppPPow @pp
-- | Reflect the exponent component of a 'PrimePower' type.
exponentPPow = snd $ ppPPow @pp
-- | The value of a 'PrimePower' type.
valuePPow = valuePP $ ppPPow @pp
-- | The totient of a 'PrimePower' type's value.
totientPPow = totientPP $ ppPPow @pp
-- | The "hat" of a 'PrimePower' type's value:
-- \( p^e \) if \( p \) is odd, \( 2^{e-1} \) otherwise.
valueHatPPow = valueHat $ valuePPow @pp
-- | The radical of a 'PrimePower' type's value.
radicalPPow = radicalPP $ ppPPow @pp
-- | The odd radical of a 'PrimePower' type's value.
oddRadicalPPow = oddRadicalPP $ ppPPow @pp

-- functions on data-level [PP]
valuePPs, totientPPs, radicalPPs, oddRadicalPPs :: [PP] -> Int
-- | Product of values of individual 'PP's
valuePPs = product . map valuePP
-- | Product of totients of individual 'PP's
totientPPs = product . map totientPP
-- | Product of radicals of individual 'PP's
radicalPPs = product . map radicalPP
-- | Product of odd radicals of individual 'PP's
oddRadicalPPs = product . map oddRadicalPP

-- functions on data-level PP
valuePP, totientPP, radicalPP, oddRadicalPP, valueHatPP :: PP -> Int
-- | The value of a prime power.
valuePP (p,e) = p^e
-- | Totient of a prime power.
totientPP (_,0) = 1
totientPP (p,e) = (p-1)*(p^(e-1))
-- | The "hat" of a prime power: \( p^e \) if \( p \) is odd, \( 2^{e-1} \)
-- otherwise.
valueHatPP = valueHat . valuePP
-- | The radical of a prime power.
radicalPP (_,0) = 1
radicalPP (p,_) = p
-- | The odd radical of a prime power.
oddRadicalPP (2,_) = 1
oddRadicalPP (p,_) = p
-- | Value of a 'PrimeBin'.
valueP :: PrimeBin -> Int
valueP (P p) = binToInt p

-- | Return \( m \) if \( m \) is odd, and \( m/2 \) otherwise.
valueHat :: Integral i => i -> i
valueHat m = if m `mod` 2 == 0 then m `div` 2 else m


-- | Template Haskell splice for the 'PrimeBin' type corresponding to a
-- given positive prime integer.  (Uses 'prime' to enforce primality
-- of the base, so should only be used on small-to-moderate-sized
-- arguments.)  This is the preferred (and only) way of constructing a
-- concrete 'PrimeBin' type (and is used to define the @Prime@\(p\) type
-- synonyms).
pType :: Int -> TypeQ
pType p
    | prime p = conT 'P `appT` binType p
    | otherwise = fail $ "pType : non-prime p " ++ show p

-- | Template Haskell splice for the 'PrimePower' type corresponding to
-- a given 'PP'.  (Calls 'pType' on the first component of its
-- argument, so should only be used on small-to-moderate-sized
-- numbers.)  This is the preferred (and only) way of constructing a
-- concrete 'PrimePower' type.
ppType :: PP -> TypeQ
ppType (p,e) = conT 'PP `appT`
               (promotedTupleT 2 `appT` pType p `appT` posType e)

-- | Template Haskell splice for the 'Factored' type corresponding to a
-- given positive integer.  Factors its argument using a naive
-- trial-division algorithm with 'primes', so should only be used on
-- small-to-moderate-sized arguments (any reasonable cyclotomic index
-- should be OK).
fType :: Int -> TypeQ
fType n = conT 'F `appT` (foldr (\pp -> appT (promotedConsT `appT` ppType pp))
                                promotedNilT $ factorize n)

-- | Template Haskell splice that defines the 'PrimeBin' type synonym
-- @Prime@\(p\) for a positive prime integer \( p \).
pDec :: Int -> DecQ
pDec p = tySynD (mkName $ "Prime" ++ show p) [] $ pType p

-- | Template Haskell splice that defines the 'PrimePower' type synonym
-- @PP@\(n\), where \( n=p^e \).
ppDec :: PP -> DecQ
ppDec pp@(p,e) = tySynD (mkName $ "PP" ++ show (p^e)) [] $ ppType pp

-- | Template Haskell splice that defines the 'Factored' type synonym
-- @F@\(n\) for a positive integer \(n\).
fDec :: Int -> DecQ
fDec n = tySynD (mkName $ 'F' : show n) [] $ fType n

-- | Converts input to its data-level 'Factored' representation.
intToFact :: Int -> Factored
intToFact m =
  let fcts = factorize m
      fcts' = map (\(p,e) -> PP (P $ intToBin p, intToPos e)) fcts
  in F fcts'

-- | Factorize a positive integer into an ordered list of its prime
-- divisors, with possible duplicates.  First argument is infinite
-- list of primes left to consider.
factorize' :: [Int] -> Int -> [Int]
factorize' _ 1 = []
factorize' ds@(d:ds') n
  | n > 1 = if d * d > n then [n]
            else let (q,r) = n `divMod` d
                 in if r == 0 then d : factorize' ds q
                    else factorize' ds' n
factorize' _ n = error $ "can't factorize non-positive n = " ++ show n

-- | Factorize a positive integer into a list of (prime,exponent)
-- pairs, in strictly increasing order by prime.
factorize :: Int -> [(Int,Int)]
factorize = map (head &&& length) . group . factorize' primes
