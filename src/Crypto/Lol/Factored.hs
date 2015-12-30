{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, GADTs,
             InstanceSigs, KindSignatures, PolyKinds, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- | This module defines types and operations for type-level
-- representation and manipulation of natural numbers, as represented
-- by their prime-power factorizations.  It relies on TH, so the
-- documentation may be difficult to read.  See source-level comments
-- for further details.

module Crypto.Lol.Factored
(
-- * Factored natural numbers
  Factored, SFactored, Fact
-- * Peano naturals
, Pos(..), SPos, Sing(SO,SS), pos
-- * Binary naturals
, Bin(..), SBin, Sing(SB1,SD0,SD1), BinC, bin
-- * Prime powers
, PrimePower(..), SPrimePower, Sing(SPP), PPow
-- * Constructors
, ppToF, sPpToF, PpToF, PToF
-- * Unwrappers
, unF, sUnF, UnF, unPP, sUnPP, UnPP, primePP, PrimePP, exponentPP, ExponentPP
-- * Arithmetic operations
, fPPMul, FPPMul, fMul, FMul, type (*)
, fDivides, FDivides, Divides, fDiv, FDiv, type (/)
, fGCD, FGCD, fLCM, FLCM, Coprime
, fOddRadical, FOddRadical
, pFree, PFree
-- * Convenient reflections
, ppsFact, valueFact, totientFact, valueHatFact, radicalFact, oddRadicalFact
, ppPPow, primePPow, exponentPPow, valuePPow, totientPPow
, valueBinC
-- * Number-theoretic laws
, transDivides, gcdDivides, lcmDivides, lcm2Divides
, pSplitTheorems, pFreeDivides
, (\\) -- re-export from Data.Constraint for convenience
-- * Utility operations (on prime powers)
, valueHat
, PP, ppToPP, valuePP, totientPP, radicalPP, oddRadicalPP
, valuePPs, totientPPs, radicalPPs, oddRadicalPPs
-- * Promoted 'Pos' types
, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10
, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20
-- * Promoted 'Bin' types
, B2, B3, B4, B5, B6, B7, B8, B9, B10
, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20
, B32, B64, B128
-- could also export the values and singletons
-- * Promoted 'Factored' types
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

import Control.Arrow ((***))
import Data.Constraint           hiding ((***))
import Data.Functor.Trans.Tagged
import Data.Singletons.Prelude   hiding ((:-))
import Data.Singletons.TH
import Language.Haskell.TH
import Unsafe.Coerce

singletons [d|
            -- | Positive naturals (1, 2, ...) in Peano representation.
            data Pos = O     -- ^ one
                     | S Pos -- ^ successor
                       deriving (Show, Eq)

            instance Ord Pos where
              compare O O          = EQ
              compare O (S _)      = LT
              compare (S _) O      = GT
              compare (S a) (S b)  = compare a b

            addPos :: Pos -> Pos -> Pos
            addPos O b      = S b
            addPos (S a) b  = S $ addPos a b

            subPos :: Pos -> Pos -> Pos
            subPos (S a) O      = a
            subPos (S a) (S b)  = subPos a b
            subPos O _          = error "Invalid call to subPos: a <= b"

           |]

posToInt :: Num z => Pos -> z
posToInt O = 1
posToInt (S a) = 1 + posToInt a

pos :: Int -> TypeQ
pos n
    | n <= 0 = error "pos requires a positive argument"
    | n == 1 = conT 'O
    | otherwise = conT 'S `appT` pos (n-1)

singletons [d|
            -- | Positive naturals in binary representation.
            data Bin = B1       -- ^ 1
                     | D0 Bin   -- ^ 2*b (double)
                     | D1 Bin   -- ^ 1 + 2*b (double and increment)
                       deriving (Show, Eq)

            instance Ord Bin where
              compare B1 B1          = EQ
              compare B1 (D0 _)      = LT
              compare B1 (D1 _)      = LT
              compare (D0 _) B1      = GT
              compare (D1 _) B1      = GT
              compare (D0 a) (D0 b)  = compare a b
              compare (D1 a) (D1 b)  = compare a b
              compare (D0 a) (D1 b)  = case compare a b of
                                       EQ -> LT
                                       LT -> LT
                                       GT -> GT
              compare (D1 a) (D0 b)  = case compare a b of
                                       EQ -> GT
                                       LT -> LT
                                       GT -> GT

           |]

-- not promotable due to numeric output

binToInt :: Num z => Bin -> z
binToInt B1 = 1
binToInt (D0 a) = 2 * binToInt a
binToInt (D1 a) = 1 + 2 * binToInt a

bin :: Int -> TypeQ
bin n
    | n <= 0 = error "bin requires a positive argument"
    | otherwise = case n `quotRem` 2 of
                    (0,1) -> conT 'B1
                    (q,0) -> conT 'D0  `appT` bin q
                    (q,1) -> conT 'D1 `appT` bin q
                 
singletons [d|

            -- | First component must be prime.
            newtype PrimePower = PP (Bin,Pos) deriving (Eq,Show)

            -- | Invariant: primes appear in strictly increasing
            -- order (no duplicates).
            newtype Factored = F [PrimePower] deriving (Eq,Show)

            -- | Unwrap 'Factored'.
            unF :: Factored -> [PrimePower]
            unF (F pps) = pps

            -- | Unwrap 'PrimePower'.
            unPP :: PrimePower -> (Bin,Pos)
            unPP (PP pp) = pp

            -- | Prime component of a 'PrimePower'.
            primePP :: PrimePower -> Bin
            primePP = fst . unPP

            -- | Exponent component of a 'PrimePower'.
            exponentPP :: PrimePower -> Pos
            exponentPP = snd . unPP

            |]

            -- singletons-2.0 doesn't work well with guards: https://github.com/goldfirere/singletons/issues/131

singletons [d|

            fPPMul :: PrimePower -> Factored -> Factored
            fMul :: Factored -> Factored -> Factored

            -- | Multiply a 'PrimePower' into a 'Factored' number.
            fPPMul pp (F pps) = F (ppMul pp pps)

            -- | Multiply two 'Factored' numbers.
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
            ppToF :: PrimePower -> Factored
            ppToF pp = F [pp]

            -- | Caller must ensure that argument is prime.
            pToF :: Bin -> Factored
            pToF p = ppToF $ PP (p, O)

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
                GT -> pp' : ppsLCM xs ys'

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
                if p == D0 B1 then ppsOddRad xs' -- D0 B1 == 2
                else PP (p,O) : ppsOddRad xs'

            |]

singletons [d|
            -- | Remove all @p@-factors from a 'Factored'.
            pFree :: Bin -> Factored -> Factored
            pFree n (F pps) = F (go pps)
              where go [] = []
                    go (pp@(PP (p,_)) : ps) =
                      if n == p then ps
                      else pp : (go ps)
            |]

-- | Type (family) synonym for division of 'Factored' types
type a / b = FDiv a b

-- | Type (family) synonym for multiplication of 'Factored' types
type a * b = FMul a b

-- convenience aliases: enforce kind, hide SingI

-- | Kind-restricted synonym for 'SingI'.
type Fact (m :: Factored) = SingI m

-- | Kind-restricted synonym for 'SingI'.
type PPow (pp :: PrimePower) = SingI pp

-- | Kind-restricted synonym for 'SingI'.
type BinC (p :: Bin) = SingI p

-- | Constraint synonym for divisibility of 'Factored' types.
type Divides m m' = (Fact m, Fact m', FDivides m m' ~ 'True)

-- | Constraint synonym for coprimality of 'Factored' types.
type Coprime m m' = (FGCD m m' ~ F '[])

-- coercions: using proxy arguments here due to compiler bugs in usage

-- coerce any divisibility relationship we want
coerceFDivs :: p m -> p' m' -> (() :- (FDivides m m' ~ 'True))
coerceFDivs _ _ = Sub $ unsafeCoerce (Dict :: Dict ())

-- coerce any GCD we want
coerceGCD :: p a -> p' a' -> p'' a'' -> (() :- (FGCD a a' ~ a''))
coerceGCD _ _ _ = Sub $ unsafeCoerce (Dict :: Dict ())

-- | Entails constraint for transitivity of division, i.e.
-- if @k|l@ and @l|m@, then @k|m@.
transDivides :: forall k l m . Proxy k -> Proxy l -> Proxy m ->
                ((k `Divides` l, l `Divides` m) :- (k `Divides` m))
transDivides k _ m = Sub Dict \\ coerceFDivs k m

-- | Entailment for divisibility by GCD:
-- if @g=GCD(m1,m2)@ then @g|m1@ and @g|m2@.
gcdDivides :: forall m1 m2 g . Proxy m1 -> Proxy m2 ->
              ((Fact m1, Fact m2, g ~ FGCD m1 m2) :-
               (g `Divides` m1, g `Divides` m2))
gcdDivides m1 m2 =
  Sub $ withSingI (sFGCD (sing :: SFactored m1) (sing :: SFactored m2))
  Dict \\ coerceFDivs (Proxy::Proxy g) m1
       \\ coerceFDivs (Proxy::Proxy g) m2

-- | Entailment for LCM divisibility:
-- if @l=LCM(m1,m2)@ then @m1|l@ and @m2|l@.
lcmDivides :: forall m1 m2 l . Proxy m1 -> Proxy m2 ->
              ((Fact m1, Fact m2, l ~ FLCM m1 m2) :-
               (m1 `Divides` l, m2 `Divides` l))
lcmDivides m1 m2 =
  Sub $ withSingI (sFLCM (sing :: SFactored m1) (sing :: SFactored m2))
  Dict \\ coerceFDivs m1 (Proxy::Proxy l)
       \\ coerceFDivs m2 (Proxy::Proxy l)

-- | Entailment for LCM divisibility:
-- the LCM of two divisors of @m@ also divides @m@.
lcm2Divides :: forall m1 m2 l m . Proxy m1 -> Proxy m2 -> Proxy m ->
               ((m1 `Divides` m, m2 `Divides` m, l ~ FLCM m1 m2) :-
                (m1 `Divides` l, m2 `Divides` l, (FLCM m1 m2) `Divides` m))
lcm2Divides m1 m2 m =
  Sub $ withSingI (sFLCM (sing :: SFactored m1) (sing :: SFactored m2))
  Dict \\ coerceFDivs (Proxy::Proxy (FLCM m1 m2)) m \\ lcmDivides m1 m2

-- | Entailment for @p@-free division:
-- if @f@ is @m@ after removing all @p@-factors, then @f|m@ and
-- @gcd(f,p)=1@.
pSplitTheorems :: forall p m f . Proxy p -> Proxy m ->
                  ((BinC p, Fact m, f ~ PFree p m) :-
                   (f `Divides` m, Coprime (PToF p) f))
pSplitTheorems _ m =
  Sub $ withSingI (sPFree (sing :: SBin p) (sing :: SFactored m))
  Dict \\ coerceFDivs (Proxy::Proxy f) m
  \\ coerceGCD (Proxy::Proxy (PToF p)) (Proxy::Proxy f) (Proxy::Proxy (F '[]))

-- | Entailment for @p@-free division:
-- if @m|m'@, then @p-free(m) | p-free(m')@.
pFreeDivides :: forall p m m' . Proxy p -> Proxy m -> Proxy m' ->
                ((BinC p, m `Divides` m') :-
                 ((PFree p m) `Divides` (PFree p m')))
pFreeDivides _ _ _ =
  Sub $ withSingI (sPFree (sing :: SBin p) (sing :: SFactored m)) $
        withSingI (sPFree (sing :: SBin p) (sing :: SFactored m')) $
        Dict \\ coerceFDivs (Proxy::Proxy (PFree p m)) (Proxy::Proxy (PFree p m'))

-- | Type synonym for @(prime, exponent)@ pair.
type PP = (Int, Int)

-- | Value-level prime-power factorization tagged by a 'Factored' type.
ppsFact :: forall m . Fact m => Tagged m [PP]
ppsFact = tag $ map ppToPP $ unF $ fromSing (sing :: SFactored m)

valueFact, totientFact, valueHatFact, radicalFact, oddRadicalFact ::
  Fact m => Tagged m Int

-- | The value of a 'Factored' type.
valueFact = valuePPs <$> ppsFact

-- | The totient of a 'Factored' type's value.
totientFact = totientPPs <$> ppsFact

-- | The "hat" of a 'Factored' type's value:
-- @\hat{m}@ is @m@ if @m@ is odd, and @m/2@ otherwise.
valueHatFact = valueHat <$> valueFact

-- | The radical (product of prime divisors) of a 'Factored' type.
radicalFact = radicalPPs <$> ppsFact

-- | The odd radical (product of odd prime divisors) of a 'Factored'
-- type.
oddRadicalFact = oddRadicalPPs <$> ppsFact

-- | Reflect a 'PrimePower' type to a 'PP' value.
ppPPow :: forall pp . PPow pp => Tagged pp PP
ppPPow = tag $ ppToPP $ fromSing (sing :: SPrimePower pp)

primePPow, exponentPPow, valuePPow, totientPPow :: PPow pp => Tagged pp Int
-- | Reflect the prime component of a 'PrimePower' type.
primePPow = fst <$> ppPPow
-- | Reflect the exponent component of a 'PrimePower' type.
exponentPPow = snd <$> ppPPow
-- | The value of a 'PrimePower' type.
valuePPow = valuePP <$> ppPPow
-- | The totient of a 'PrimePower' type's value.
totientPPow = totientPP <$> ppPPow

-- | The value of a 'Bin' type.
valueBinC :: forall p . BinC p => Tagged p Int
valueBinC = tag $ binToInt $ fromSing (sing :: SBin p)

-- | Return @m@ if @m@ is odd, and @m/2@ otherwise.
valueHat :: Integral i => i -> i
valueHat m = if m `mod` 2 == 0 then m `div` 2 else m

-- | Convert to an @Int@ pair.
ppToPP :: PrimePower -> PP
ppToPP = (binToInt *** posToInt) . unPP

valuePP, totientPP, radicalPP, oddRadicalPP :: PP -> Int
-- | The value of a prime power.
valuePP (p,e) = p^e

-- | Totient of a prime power.
totientPP (_,0) = 1
totientPP (p,e) = (p-1)*(p^(e-1))

-- | The prime component of a prime power.
radicalPP (_,0) = 1
radicalPP (p,_) = p

-- | The odd radical of a prime power.
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

singletons [d|

            p1 = O
            p2 = S p1
            p3 = S p2
            p4 = S p3
            p5 = S p4
            p6 = S p5
            p7 = S p6
            p8 = S p7
            p9 = S p8
            p10 = S p9
            p11 = S p10
            p12 = S p11
            p13 = S p12
            p14 = S p13
            p15 = S p14
            p16 = S p15
            p17 = S p16
            p18 = S p17
            p19 = S p18
            p20 = S p19

           |]

singletons [d|
            
            b2 = D0 B1
            b3 = D1 B1
            b4 = D0 b2
            b5 = D1 b2
            b6 = D0 b3
            b7 = D1 b3
            b8 = D0 b4
            b9 = D1 b4
            b10 = D0 b5
            b11 = D1 b5
            b12 = D0 b6
            b13 = D1 b6
            b14 = D0 b7
            b15 = D1 b7
            b16 = D0 b8
            b17 = D1 b8
            b18 = D0 b9
            b19 = D1 b9
            b20 = D0 b10
            b32 = D0 b16
            b64 = D0 b32
            b128 = D0 b64

           |]

singletons [d|

            f1 = F []
            f2 = pToF b2
            f3 = pToF b3
            f4 = f2 `fMul` f2
            f5 = pToF b5
            f6 = f2 `fMul` f3
            f7 = pToF b7
            f8 = f2 `fMul` f4
            f9 = f3 `fMul` f3
            f10 = f2 `fMul` f5
            f11 = pToF b11
            f12 = f4 `fMul` f3
            f13 = pToF b13
            f14 = f2 `fMul` f7
            f15 = f3 `fMul` f5
            f16 = f2 `fMul` f8
            f17 = pToF b17
            f18 = f2 `fMul` f9
            f19 = pToF b19
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
