{-# LANGUAGE DataKinds, FlexibleContexts, NoImplicitPrelude, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies #-}


import Gen
import Utils
import Harness.SHE
import Benchmarks hiding (hideArgs)

import Control.Applicative
import Control.Monad.Random
import Control.Monad.State
import Crypto.Random.DRBG

import Crypto.Lol hiding (CT)
import Crypto.Lol.Applications.SymmSHE
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as CT
import Crypto.Lol.Types.Random

import qualified Criterion as C
import qualified Criterion.Main as C


-- benchmarks two functions, each with two different sets of parameters
main :: IO ()
main = C.defaultMain =<< (sequence [
  hideArgs bench_mul (Proxy::Proxy '(RT,F7,F21,Zq 8,Zq 43)),
  hideArgs bench_mul (Proxy::Proxy '(RT,F3,F21,Zq 4,Zq 43)),
  hideArgs bench_enc (Proxy::Proxy '(RT,F7,F21,Zq 8,Zq 43,HashDRBG)),
  hideArgs bench_enc (Proxy::Proxy '(RT,F3,F21,Zq 4,Zq 43,HashDRBG))
  ])

-- generates random arguments to functions
-- Note: The generation time is *not* included in the benchmark time.
hideArgs :: forall a rnd bnch . 
  (GenArgs (StateT (Maybe (SKOf a)) rnd) bnch, Monad rnd, ShowType a,
   ResultOf bnch ~ Bench a)
  => bnch -> Proxy a -> rnd Benchmark
hideArgs f p = (C.bench (showType p) . unbench) <$> 
  (evalStateT (genArgs f) (Nothing :: Maybe (SKOf a)))

-- benchmark ciphertext multiplication (n.b. for good ciphertext moduli, arguments are generated in the CRT basis,
-- so this benchmark is really just a zipWith. for bad moduli, arguments are generated in the powerful basis, so
-- we have to convert both arguments to CRT, then do a zipWith)
bench_mul :: (Ring (CT m zp (Cyc t m' zq)), NFData (CT m zp (Cyc t m' zq)))
  => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq) -> Bench '(t,m,m',zp,zq)
bench_mul a = bench (*a)

-- this *always* performs two CRTs and a zipWith, no matter good or bad modulus
-- Note that the "advisePow"s go *outside* the `bench` function, so the (possible)
-- conversion to powerful basis is *not* timed. However, the (implicity) conversion
-- to CRT before multiplication *is* timed.
bench_mul2 :: (Ring (CT m zp (Cyc t m' zq)), NFData (CT m zp (Cyc t m' zq)))
  => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq) -> Bench '(t,m,m',zp,zq)
bench_mul2 a b = 
  let a' = advisePow a
      b' = advisePow b
  in bench (*a') b'

-- use benchIO because the thing we are benchmarking is monadic 
bench_enc :: forall t m m' z zp zq gen . (EncryptCtx t m m' z zp zq, CryptoRandomGen gen, z ~ LiftOf zp, NFElt zp, NFElt zq)
  => SK (Cyc t m' z) -> PT (Cyc t m zp) -> Bench '(t,m,m',zp,zq,gen)
bench_enc sk pt = benchIO $ do
  gen <- newGenIO
  return $ evalRand (encrypt sk pt :: Rand (CryptoRand gen) (CT m zp (Cyc t m' zq))) gen