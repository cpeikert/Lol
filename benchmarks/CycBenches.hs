{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, 
             GADTs, MultiParamTypeClasses,NoImplicitPrelude, RankNTypes, 
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, 
             TypeOperators, UndecidableInstances #-}

module CycBenches (cycBenches) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Types.Random
import Crypto.Random.DRBG

import Data.Singletons
import Data.Promotion.Prelude.List
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar

import Utils

cycBenches :: (MonadRandom rnd) => rnd Benchmark
cycBenches = bgroupRnd "Cyc"
  [bgroupRnd "*"       $ benchBasic $ wrapCyc bench_mul,
   bgroupRnd "crt"     $ benchBasic $ wrapCyc bench_crt,
   bgroupRnd "crtInv"  $ benchBasic $ wrapCyc bench_crtInv,
   bgroupRnd "l"       $ benchBasic $ wrapCyc bench_l,
   bgroupRnd "*g Pow"  $ benchBasic $ wrapCyc bench_mulgPow,
   bgroupRnd "*g CRT"  $ benchBasic $ wrapCyc bench_mulgCRT,
   bgroupRnd "lift"    $ benchLift  $ wrapCyc bench_liftPow,
   bgroupRnd "error"   $ benchError $ wrapError $ bench_errRounded 0.1,
   bgroupRnd "twace"   $ benchTwoIdx $ wrapTwace bench_twacePow,
   bgroupRnd "embed"   $ benchTwoIdx $ wrapEmbed bench_embedPow
   -- sanity checks
   --, bgroupRnd "^2" $ groupC $ wrap1Arg bench_sq,             -- should take same as bench_mul
   --, bgroupRnd "id2" $ groupC $ wrap1Arg bench_advisePowPow -- should take a few nanoseconds: this is a no-op
  ]

-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: (BasicCtx t m r) => Cyc t m r -> Cyc t m r -> NFValue
bench_mul a b = 
  let a' = adviseCRT a
      b' = adviseCRT b
  in nf (a' *) b'
{-# SPECIALIZE bench_mul :: Cyc RT F288 (ZqBasic 577 Int64) -> Cyc RT F288 (ZqBasic 577 Int64) -> NFValue #-}

-- convert input from Pow basis to CRT basis
bench_crt :: (BasicCtx t m r) => Cyc t m r -> NFValue
bench_crt x = let y = advisePow x in nf adviseCRT y

-- convert input from CRT basis to Pow basis
bench_crtInv :: (BasicCtx t m r) => Cyc t m r -> NFValue
bench_crtInv x = let y = adviseCRT x in nf advisePow y

-- convert input from Dec basis to Pow basis
bench_l :: (BasicCtx t m r) => Cyc t m r -> NFValue
bench_l x = let y = adviseDec x in nf advisePow y

-- lift an element in the Pow basis
bench_liftPow :: forall t m r . (LiftCtx t m r) => Cyc t m r -> NFValue
bench_liftPow x = let y = advisePow x in nf (liftCyc Pow :: Cyc t m r -> Cyc t m (LiftOf r)) y

-- multiply by g when input is in Pow basis
bench_mulgPow :: (BasicCtx t m r) => Cyc t m r -> NFValue
bench_mulgPow x = let y = advisePow x in nf mulG y

-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => Cyc t m r -> NFValue
bench_mulgCRT x = let y = adviseCRT x in nf mulG y

-- generate a rounded error term
bench_errRounded :: forall t m r gen . (LiftCtx t m r, CryptoRandomGen gen) 
  => Double -> Proxy gen -> Proxy (t m r) -> NFValue
bench_errRounded v _ _ = nfIO $ do
  gen <- newGenIO
  return $ evalRand (errorRounded v :: Rand (CryptoRand gen) (Cyc t m (LiftOf r))) gen

bench_twacePow :: forall t m m' r . (TwoIdxCtx t m m' r) 
  => Proxy m -> Cyc t m' r -> NFValue
bench_twacePow _ x = let y = advisePow x in nf (twace :: Cyc t m' r -> Cyc t m r) y

bench_embedPow :: forall t m m' r . (TwoIdxCtx t m m' r) 
  => Proxy m' -> Cyc t m r -> NFValue
bench_embedPow _ x = let y = advisePow x in nf (embed :: Cyc t m r -> Cyc t m' r) y
{-
-- sanity check: this test should take the same amount of time as bench_mul
-- if it takes less, then random element generation is being counted!
bench_sq :: (CElt t r, Fact m) => Cyc t m r -> NFValue
bench_sq a = nf (a *) a

-- sanity check: this should be a no-op
bench_advisePowPow :: (CElt t r, Fact m) => Cyc t m r -> NFValue
bench_advisePowPow x = let y = advisePow x in nf advisePow y
-}

type Tensors = '[{-CT,-}RT]
type MM'RCombos = 
  '[ --'(F4, F128, Zq 257),
     --'(F1, PToF Prime281, Zq 563),
     --'(F12, F32 * F9, Zq 512),
     '(F12, F32 * F9, Zq 577)
     --'(F12, F32 * F9, Zq (577 ** 1153)),
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017))
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593))
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169)),
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457)),
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337)),
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337 ** 7489)),
     --'(F12, F32 * F9 * F25, Zq 14401)
    ]

-- EAC: must be careful where we use Nub: apparently TypeRepStar doesn't work well with the Tensor constructors
type AllParams = ( '(,) <$> Tensors) <*> (Nub (Map RemoveM MM'RCombos))
type LiftParams = ( '(,) <$> Tensors) <*> (Nub (Filter Liftable (Map RemoveM MM'RCombos)))

data Liftable :: TyFun (Factored, *) Bool -> *
type instance Apply Liftable '(m',r) = Int64 :== (LiftOf r)

data RemoveM :: TyFun (Factored, Factored, *) (Factored, *) -> *
type instance Apply RemoveM '(m,m',r) = '(m',r)


data BasicCtxD
type BasicCtx t m r = (CElt t r, Fact m, ShowArgs '(t,m,r))
instance (params `Satisfy` BasicCtxD, BasicCtx t m r) => ( '(t, '(m,r)) ': params) `Satisfy` BasicCtxD where
  data ArgsCtx BasicCtxD where
    BC :: (BasicCtx t m r) => Proxy '(t,m,r) -> ArgsCtx BasicCtxD
  runAll _ f = (f $ BC (Proxy::Proxy '(t,m,r))) : (runAll (Proxy::Proxy params) f)

hideTMR :: (forall t m r . (BasicCtx t m r) => Proxy '(t,m,r) -> rnd Benchmark) -> ArgsCtx BasicCtxD -> rnd Benchmark
hideTMR f (BC p) = f p

wrapCyc :: (Functor rnd, Benchmarkable rnd (Cyc t m r -> bnch), ShowArgs '(t,m,r)) 
  => (Cyc t m r -> bnch) -> Proxy '(t,m,r) -> rnd Benchmark
wrapCyc f p = bench (showArgs p) <$> genArgs f

benchBasic :: (forall t m r . (BasicCtx t m r) => Proxy '(t,m,r) -> rnd Benchmark) -> [rnd Benchmark]
benchBasic g = runAll (Proxy::Proxy AllParams) $ hideTMR g


data LiftCtxD
type LiftCtx t m r = (BasicCtx t m r, CElt t (LiftOf r), Lift' r, ToInteger (LiftOf r))
instance (params `Satisfy` LiftCtxD, LiftCtx t m r) => ( '(t, '(m,r)) ': params) `Satisfy` LiftCtxD  where
  data ArgsCtx LiftCtxD where
    LC :: (LiftCtx t m r) => Proxy '(t,m,r) -> ArgsCtx LiftCtxD
  runAll _ f = (f $ LC (Proxy::Proxy '(t,m,r))) : (runAll (Proxy::Proxy params) f)

hideLift :: (forall t m r . (LiftCtx t m r) => Proxy '(t,m,r) -> rnd Benchmark) -> ArgsCtx LiftCtxD -> rnd Benchmark
hideLift f (LC p) = f p

benchLift :: (forall t m r . (LiftCtx t m r) => Proxy '(t,m,r) -> rnd Benchmark) -> [rnd Benchmark]
benchLift g = runAll (Proxy::Proxy LiftParams) $ hideLift g

wrapError ::(LiftCtx t m r, Monad rnd, CryptoRandomGen gen)
  => (Proxy gen -> Proxy (t m r) -> NFValue)
     -> Proxy gen -> Proxy '(t,m,r) -> rnd Benchmark
wrapError f _ p = return $ bench (showArgs p) $ f Proxy Proxy

benchError :: (Monad rnd)
  => (forall t m r gen . (LiftCtx t m r, CryptoRandomGen gen) => Proxy gen -> Proxy '(t,m,r) -> rnd Benchmark) 
     -> [rnd Benchmark]
benchError g = [
  bgroupRnd "HashDRBG" $ benchLift $ g (Proxy::Proxy HashDRBG),
  bgroupRnd "SysRand"  $ benchLift $ g (Proxy::Proxy SystemRandom)]


data TwoIdxCtxD
type TwoIdxCtx t m m' r = (m `Divides` m', CElt t r, ShowArgs '(t,m,m',r))

instance (params `Satisfy` TwoIdxCtxD, TwoIdxCtx t m m' r) => ( '(t, '(m,m',r)) ': params) `Satisfy` TwoIdxCtxD where
  data ArgsCtx TwoIdxCtxD where
    TI :: (TwoIdxCtx t m m' r) => Proxy '(t,m,m',r) -> ArgsCtx TwoIdxCtxD
  runAll _ f = (f $ TI (Proxy::Proxy '(t,m,m',r))) : (runAll (Proxy::Proxy params) f)

hideTMM'R :: (forall t m m' r . (TwoIdxCtx t m m' r) => Proxy '(t,m,m',r) -> rnd Benchmark) -> ArgsCtx TwoIdxCtxD -> rnd Benchmark
hideTMM'R f (TI p) = f p

wrapTwace :: (Fact m, Functor rnd, Benchmarkable rnd (Cyc t m' r -> NFValue), ShowArgs '(t,m,m',r))
  => (Proxy m -> Cyc t m' r -> NFValue) -> Proxy '(t,m,m',r) -> rnd Benchmark
wrapTwace f p = bench (showArgs p) <$> genArgs (f Proxy)

wrapEmbed :: (Fact m', Functor rnd, Benchmarkable rnd (Cyc t m r -> NFValue), ShowArgs '(t,m,m',r))
  => (Proxy m' -> Cyc t m r -> NFValue) -> Proxy '(t,m,m',r) -> rnd Benchmark
wrapEmbed f p = bench (showArgs p) <$> genArgs (f Proxy)

benchTwoIdx :: (forall t m m' r . (TwoIdxCtx t m m' r) => Proxy '(t,m,m',r) -> rnd Benchmark) -> [rnd Benchmark]
benchTwoIdx f = runAll (Proxy::Proxy (( '(,) <$> Tensors) <*> MM'RCombos)) $ hideTMM'R f
