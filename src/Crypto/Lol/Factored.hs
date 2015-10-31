{-# LANGUAGE ConstraintKinds, DataKinds,
             GADTs, KindSignatures, PolyKinds,
             ScopedTypeVariables, TemplateHaskell, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

-- | This file defines types and operations for type-level
-- representation and manipulation of factored integers. It relies on
-- TH, so the documentation will be difficult to read. See comments
-- for better documentation.

module Crypto.Lol.Factored
(
-- * Factored natural numbers
  Factored(..), SFactored, Fact
-- * Prime powers
, PrimePower(..), SPrimePower, PPow, Sing (SPP)
-- * Naturals
, Nat, NatC, PrimeNat, Prime
-- * Constructors
, toPP, sToPP, ToPP, ppToF, sPpToF, PpToF, PToF
-- * Unwrappers
, unF, sUnF, UnF, unPP, sUnPP, UnPP, primePP, PrimePP, exponentPP, ExponentPP
-- * Arithmetic operations
, fPPMul, FPPMul, fMul, FMul, type (*)
, fDivides, FDivides, Divides, fDiv, FDiv, type (/)
, fGCD, FGCD, fLCM, FLCM, Coprime
, fOddRadical, FOddRadical
, pFree, PFree
-- * Reflections
, ppsFact, valueFact, totientFact, valueHatFact, radicalFact, oddRadicalFact
, ppPPow, primePPow, exponentPPow, valuePPow, totientPPow
, valueNatC
-- * Number-theoretic laws
, transDivides, gcdDivides, lcmDivides, lcm2Divides
, pSplitTheorems, pFreeDivides
, (\\) -- re-export from Data.Constraint for convenience
-- * Utility operations (on prime powers)
, valueHat
, PP, ppToPP, valuePP, totientPP, radicalPP, oddRadicalPP
, valuePPs, totientPPs, radicalPPs, oddRadicalPPs
-- * Type synonyms (not type families)
, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10
, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20
, F21, F22, F24, F25, F26, F27, F28, F30
, F32, F33, F34, F35, F36, F38, F39
, F40, F42, F44, F45, F48, F49
, F50, F51, F52, F54, F55, F56, F57
, F60, F63, F64, F65, F66, F68
, F70, F72, F75, F76, F77, F78, F80, F81, F84, F85, F88
, F90, F91, F95, F96, F98, F99
, F128, F256, F512, F1024, F2048
) where

import Data.Constraint hiding ((***))
import Data.Functor.Trans.Tagged
import Data.Singletons.Prelude hiding (sMin, sMax, MinSym0, MaxSym0, (:-))
import Data.Singletons.TH
import Data.Type.Natural         as N hiding ((:-))
import Data.Typeable

import Control.Arrow ((***))
import Unsafe.Coerce

-- | Copied from Data.Type.Natural because the data-level version
-- is not exported there.
(<<=) :: Nat -> Nat -> Bool
Z   <<= _   = True
S _ <<= Z   = False
S n <<= S m = n <<= m

singletons [d|

            -- Invariant: first component is prime, second component
            -- (the exponent) is positive (nonzero)
            newtype PrimePower = PP (Nat,Nat) deriving (Eq,Show,Typeable)

            -- List invariant: primes appear in strictly increasing
            -- order (no duplicates)
            newtype Factored = F [PrimePower] deriving (Eq,Show,Typeable)

            -- unwrap 'Factored'
            unF :: Factored -> [PrimePower]
            unF (F pps) = pps

            -- unwrap 'PrimePower'
            unPP :: PrimePower -> (Nat,Nat)
            unPP (PP pp) = pp

            -- grab individual components of a 'PrimePower'
            primePP, exponentPP :: PrimePower -> Nat
            primePP = fst . unPP
            exponentPP = snd . unPP

            |]

-- SMART CONSTRUCTORS
singletons [d|

            fPPMul :: PrimePower -> Factored -> Factored
            fMul :: Factored -> Factored -> Factored

            -- constructor implementations
            -- multiply a new 'PrimePower' into a 'Factored' number
            fPPMul (PP(_,Z)) y = y -- throw away trivial prime power
            fPPMul pp@(PP(_,S _)) (F pps) = F (ppMul pp pps)

            -- multiply two 'Factored' numbers
            fMul (F pps1) (F pps2) = F (ppsMul pps1 pps2)

            -- helper functions (not for export)

            -- keeps primes in sorted order; merges duplicates

            -- EAC: Singletons(?) doesn't play well with pattern synonyms (e.g. x@(PP(p,e)))
            -- when compiling with -O2
            -- reported as #10924
            -- singletons-2.0 doesn't work well with guards: https://github.com/goldfirere/singletons/issues/131
            ppMul :: PrimePower -> [PrimePower] -> [PrimePower]
            ppMul x [] = [x]
            ppMul (PP(p,e)) (PP (p',e'):pps') =
              if p == p' then PP(p,e + e'):pps'
              else if p <<= p' then (PP(p,e)):(PP (p',e'):pps')
              else (PP(p',e')):ppMul (PP(p,e)) pps'

            ppsMul :: [PrimePower] -> [PrimePower] -> [PrimePower]
            ppsMul [] ys = ys
            ppsMul (pp:pps) ys = ppsMul pps (ppMul pp ys)

            |]

-- ARITHMETIC OPERATIONS
singletons [d|
            -- Smart constructor that checks that the first arg is
            -- prime (< 20) and the second arg is positive
            toPP :: Nat -> Nat -> PrimePower
            toPP p e | primeNat p && (n1 <<= e) = PP (p,e)

            -- EAC: isn't there a singletons promotion for 'F'
            -- that could replace this function?
            ppToF :: PrimePower -> Factored
            ppToF pp = F [pp]

            primeToF :: Nat -> Factored
            primeToF p | primeNat p = ppToF $ PP (p, n1)
            
            fGCD, fLCM :: Factored -> Factored -> Factored
            fDivides :: Factored -> Factored -> Bool
            fDiv :: Factored -> Factored -> Factored
            fOddRadical :: Factored -> Factored
            
            -- can't pattern-match on n*, but can test equality
            primeNat n
              | n==n2 = True
              | n==n3 = True
              | n==n5 = True
              | n==n7 = True
              | n==n11 = True
              | n==n13 = True
              | n==n17 = True
              | n==n19 = True
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
            ppsGCD [] [] = []
            ppsGCD [] (_:_) = []
            ppsGCD (_:_) [] = []
            ppsGCD (PP (p,e) : xs') (PP (p',e') : ys') =
              if p == p' then PP (p,N.min e e') : ppsGCD xs' ys'
              else if p <<= p' then ppsGCD xs' (PP (p',e') : ys')
              else ppsGCD (PP (p,e) : xs') ys'

            ppsLCM :: [PrimePower] -> [PrimePower] -> [PrimePower]
            ppsLCM [] [] = []
            ppsLCM [] ys@(_:_) = ys
            ppsLCM xs@(_:_) [] = xs
            ppsLCM ((PP (p,e)) : xs') ((PP (p',e')) : ys') =
              if p == p' then PP (p,N.max e e') : ppsLCM xs' ys'
              else if p <<= p' then (PP (p,e)) : ppsLCM xs' ((PP (p',e')) : ys')
              else (PP (p',e')) : ppsLCM ((PP (p,e)) : xs') ys'

            ppsDivides :: [PrimePower] -> [PrimePower] -> Bool
            ppsDivides [] _ = True
            ppsDivides (_:_) [] = False
            ppsDivides (PP (p,e) : xs') (PP (p',e') : ys') =
              if p == p' then (e <<= e') && ppsDivides xs' ys'
              else not (p <<= p') && ppsDivides (PP (p,e) : xs') ys'

            ppsDiv :: [PrimePower] -> [PrimePower] -> [PrimePower]
            ppsDiv xs [] = xs
            ppsDiv ((PP (p,e)) : xs') (PP (p',e') : ys') =
              if p == p' && e' == e then ppsDiv xs' ys'
              else if p == p' && e' <<= e then PP (p,e-e') : ppsDiv xs' ys'
              else if p <<= p' then (PP (p,e)) : ppsDiv xs' (PP (p',e') : ys')
              else error "type error in ppsDiv"                -- if p' <<= p then it's an error

            ppsOddRad :: [PrimePower] -> [PrimePower]
            ppsOddRad [] = []
            ppsOddRad (PP ((S (S Z)),_) : xs') = ppsOddRad xs'
            -- need to expand to avoid overlapping with previous case
            ppsOddRad (PP (p@(S (S (S _))),_) : xs') = PP (p,n1) : ppsOddRad xs'

            |]

singletons [d|
            -- removes all @p@-factors from a 'Factored'
            pFree :: Nat -> Factored -> Factored
            pFree n (F pps) = F (go pps)
              where go [] = []
                    go (pp@(PP (p,_)) : ps) =
                      if n == p then ps
                      else pp : (go ps)
            |]

singletons [d|

            f1 = F []
            f2 = primeToF n2
            f3 = primeToF n3
            f4 = f2 `fMul` f2
            f5 = primeToF n5
            f6 = f2 `fMul` f3
            f7 = primeToF n7
            f8 = f2 `fMul` f4
            f9 = f3 `fMul` f3
            f10 = f2 `fMul` f5
            f11 = primeToF n11
            f12 = f4 `fMul` f3
            f13 = primeToF n13
            f14 = f2 `fMul` f7
            f15 = f3 `fMul` f5
            f16 = f2 `fMul` f8
            f17 = primeToF n17
            f18 = f2 `fMul` f9
            f19 = primeToF n19
            f20 = f2 `fMul` f10
            f21 = f3 `fMul` f7
            f22 = f2 `fMul` f11
            f24 = f2 `fMul` f12
            f25 = f5 `fMul` f5
            f26 = f2 `fMul` f13
            f27 = f3 `fMul` f9
            f28 = f2 `fMul` f14
            f30 = f2 `fMul` f15
            f32 = f2 `fMul` f16
            f33 = f3 `fMul` f11
            f34 = f2 `fMul` f17
            f35 = f5 `fMul` f7
            f36 = f2 `fMul` f18
            f38 = f2 `fMul` f19
            f39 = f3 `fMul` f13
            f40 = f2 `fMul` f20
            f42 = f2 `fMul` f21
            f44 = f2 `fMul` f22
            f45 = f3 `fMul` f15
            f48 = f2 `fMul` f24
            f49 = f7 `fMul` f7
            f50 = f2 `fMul` f25
            f51 = f3 `fMul` f17
            f52 = f2 `fMul` f26
            f54 = f2 `fMul` f27
            f55 = f5 `fMul` f11
            f56 = f2 `fMul` f28
            f57 = f3 `fMul` f19
            f60 = f2 `fMul` f30
            f63 = f3 `fMul` f21
            f64 = f2 `fMul` f32
            f65 = f5 `fMul` f13
            f66 = f2 `fMul` f33
            f68 = f2 `fMul` f34
            f70 = f2 `fMul` f35
            f72 = f2 `fMul` f36
            f75 = f3 `fMul` f25
            f76 = f2 `fMul` f38
            f77 = f7 `fMul` f11
            f78 = f2 `fMul` f39
            f80 = f2 `fMul` f40
            f81 = f3 `fMul` f27
            f84 = f2 `fMul` f42
            f85 = f5 `fMul` f17
            f88 = f2 `fMul` f44
            f90 = f2 `fMul` f45
            f91 = f7 `fMul` f13
            f95 = f5 `fMul` f19
            f96 = f2 `fMul` f48
            f98 = f2 `fMul` f49
            f99 = f9 `fMul` f11
            f128 = f2 `fMul` f64
            f256 = f2 `fMul` f128
            f512 = f2 `fMul` f256
            f1024 = f2 `fMul` f512
            f2048 = f2 `fMul` f1024
            |]

-- | Type (family) synonym for division of 'Factored' types
type a / b = FDiv a b

-- | Type (family) synonym for multiplication of 'Factored' types
type a * b = FMul a b

-- | Type (family) synonym to create a Factored from a prime Nat
type PToF p = PpToF (ToPP p N1)

-- convenience aliases: enforce kind, hide SingI

-- | Kind-restricted synonym for 'SingI'. Use this in constraints 
-- for types requiring a 'Factored' type.
type Fact (m :: Factored) = SingI m

-- | Kind-restricted synonym for 'SingI'. Use this in constraints 
-- for types requiring a 'PrimePower' type.
type PPow (pp :: PrimePower) = SingI pp

-- | Kind-restricted synonym for 'SingI'. Use this in constraints 
-- for types requiring a 'Nat' type.
type NatC (p :: Nat) = SingI p

type Prime p = (NatC p, PrimeNat p ~ 'True)

-- | Constraint synonym for divisibility of 'Factored' types
type Divides m m' = (Fact m, Fact m', FDivides m m' ~ 'True)

-- | Constraint synonym for coprimality of 'Factored' types
type Coprime m m' = (FGCD m m' ~ F1)

-- coercions: using proxy arguments here due to compiler bugs in usage

-- coerce any divisibility relationship we want
coerceFDivs :: p m -> p' m' -> (() :- (FDivides m m' ~ True))
coerceFDivs _ _ = Sub $ unsafeCoerce (Dict :: Dict ())

-- coerce any GCD we want
coerceGCD :: p a -> p' a' -> p'' a'' -> (() :- (FGCD a a' ~ a''))
coerceGCD _ _ _ = Sub $ unsafeCoerce (Dict :: Dict ())

-- | Entails constraint for transitivity of division, i.e.
-- if @k|l@ and @l|m@, then @k|m@.
transDivides :: forall k l m . Proxy k -> Proxy l -> Proxy m ->
                ((k `Divides` l, l `Divides` m) :- (k `Divides` m))
transDivides k _ m = Sub Dict \\ coerceFDivs k m

-- | Entails constraint for divisibility by GCD, i.e.
-- if @g=GCD(m1,m2)@, then @g|m1@ and @g|m2@.
gcdDivides :: forall m1 m2 g . Proxy m1 -> Proxy m2 ->
              ((Fact m1, Fact m2, g ~ FGCD m1 m2) :-
               (g `Divides` m1, g `Divides` m2))
gcdDivides m1 m2 =
  Sub $ withSingI (sFGCD (sing :: SFactored m1) (sing :: SFactored m2))
  Dict \\ coerceFDivs (Proxy::Proxy g) m1
       \\ coerceFDivs (Proxy::Proxy g) m2

-- | Entails constraint for LCM divisibility, i.e.
-- if @l=LCM(m1,m2)@, then @m1|l@ and @m2|l@.
lcmDivides :: forall m1 m2 l . Proxy m1 -> Proxy m2 ->
              ((Fact m1, Fact m2, l ~ FLCM m1 m2) :-
               (m1 `Divides` l, m2 `Divides` l))
lcmDivides m1 m2 = 
  Sub $ withSingI (sFLCM (sing :: SFactored m1) (sing :: SFactored m2))
  Dict \\ coerceFDivs m1 (Proxy::Proxy l)
       \\ coerceFDivs m2 (Proxy::Proxy l)

-- | Entails constraint for LCM divisibility, i.e.
-- the LCM of two divisors of @m@ also divides @m@.
lcm2Divides :: forall m1 m2 l m . Proxy m1 -> Proxy m2 -> Proxy m ->
               ((m1 `Divides` m, m2 `Divides` m, l ~ FLCM m1 m2) :-
                (m1 `Divides` l, m2 `Divides` l, (FLCM m1 m2) `Divides` m))
lcm2Divides m1 m2 m = 
  Sub $ withSingI (sFLCM (sing :: SFactored m1) (sing :: SFactored m2))
  Dict \\ coerceFDivs (Proxy::Proxy (FLCM m1 m2)) m \\ lcmDivides m1 m2

-- | Entails basic facts for @p@-free division, i.e.
-- if @f@ is @m@ after removing all @p@-factors, then @f|m@ and
-- @gcd(f,p)=1@
pSplitTheorems :: forall p m f . Proxy p -> Proxy m ->
                  ((NatC p, Fact m, f ~ PFree p m) :-
                   (f `Divides` m, Coprime (PToF p) f))
pSplitTheorems _ m =
  Sub $ withSingI (sPFree (sing :: SNat p) (sing :: SFactored m))
  Dict \\ coerceFDivs (Proxy::Proxy f) m 
  \\ coerceGCD (Proxy::Proxy (PToF p)) (Proxy::Proxy f) (Proxy::Proxy F1)

-- | Entails basic facts for @p@-free division, i.e.,
-- if @m|m'@, then @p-free(m) | p-free(m')@
pFreeDivides :: forall p m m' . Proxy p -> Proxy m -> Proxy m' ->
                ((NatC p, m `Divides` m') :-
                 ((PFree p m) `Divides` (PFree p m')))
pFreeDivides _ _ _ =
  Sub $ withSingI (sPFree (sing :: SNat p) (sing :: SFactored m)) $
        withSingI (sPFree (sing :: SNat p) (sing :: SFactored m')) $
        Dict \\ coerceFDivs (Proxy::Proxy (PFree p m)) (Proxy::Proxy (PFree p m'))

-- | Type synonym for @(prime :: Int, exponent :: Int)@ pair
type PP = (Int, Int)

-- | Value-level prime-power factorization tagged by a 'Factored' type.
ppsFact :: forall m . (Fact m) => Tagged m [PP]
ppsFact = tag $ map ppToPP $ unF $ fromSing (sing :: SFactored m)

valueFact, totientFact, valueHatFact, radicalFact, oddRadicalFact ::
  (Fact m) => Tagged m Int

-- | @Int@ representing the value of a 'Factored' type
valueFact = valuePPs <$> ppsFact

-- | @Int@ representing the totient of a 'Factored' type's value
totientFact = totientPPs <$> ppsFact

-- | @Int@ representing the "hat" of a 'Factored' type's value @m@:
-- @m@, if @m@ is odd, or @m/2@ otherwise.
valueHatFact = valueHat <$> valueFact

-- | @Int@ representing the radical (product of prime divisors)
-- of a 'Factored' type
radicalFact = radicalPPs <$> ppsFact

-- | @Int@ representing the odd radical (product of odd prime divisors)
-- of a 'Factored' type
oddRadicalFact = oddRadicalPPs <$> ppsFact

-- | Reflects a 'PrimePower' type to a 'PP' value
ppPPow :: forall pp . (PPow pp) => Tagged pp PP
ppPPow = tag $ ppToPP $ fromSing (sing :: SPrimePower pp)

primePPow, exponentPPow, valuePPow, totientPPow :: (PPow pp) => Tagged pp Int
-- | Reflects the prime component of a 'PrimePower' type
primePPow = fst <$> ppPPow
-- | Reflects the exponent component of a 'PrimePower' type
exponentPPow = snd <$> ppPPow
-- | @Int@ representing the value of a 'PrimePower' type
valuePPow = valuePP <$> ppPPow
-- | @Int@ representing the totient of a 'PrimePower' type's value
totientPPow = totientPP <$> ppPPow

-- | @Int@ representing the value of a 'Nat'
valueNatC :: forall p . (NatC p) => Tagged p Int
valueNatC = tag $ sNatToInt (sing :: SNat p)

-- | Returns @m@, if @m@ is odd, or @m/2@ otherwise
valueHat :: (Integral i) => i -> i
valueHat m = if m `mod` 2 == 0 then m `div` 2 else m

-- | Converts a 'Nat' prime-power pair to an @Int@ prime-power pair
ppToPP :: PrimePower -> PP
ppToPP = (natToInt *** natToInt) . unPP

valuePP, totientPP, radicalPP, oddRadicalPP :: PP -> Int
-- | Evaluates a prime-power pair @(p,e)@ to @p^e@
valuePP (p,e) = p^e

-- | Euler's totient function of a prime-power pair
totientPP (_,0) = 1
totientPP (p,e) = (p-1)*(p^(e-1))

-- | The prime component of a prime-power pair
radicalPP (_,0) = 1
radicalPP (p,_) = p

-- | The odd radical of a prime-power pair (p,_):
-- p if p is odd,
-- 1 if p==2
oddRadicalPP (_,0) = 1
oddRadicalPP (2,_) = 1
oddRadicalPP (p,_) = p

valuePPs, totientPPs, radicalPPs, oddRadicalPPs :: [PP] -> Int
-- | Product of values of individual 'PP's
valuePPs = product . map valuePP
-- | Product of totients of individual 'PP's
totientPPs = product . map totientPP
-- | Product of radicals of individual 'PP's
radicalPPs = product . map radicalPP
-- | Product of odd radicals of individual 'PP's
oddRadicalPPs = product . map oddRadicalPP
