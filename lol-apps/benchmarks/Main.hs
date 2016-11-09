{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Control.Monad.Random

import Crypto.Lol (Cyc)
import Crypto.Lol.Applications.SymmSHE hiding (CT)
import Crypto.Lol.Benchmarks
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Factored
import Crypto.Lol.Gadget
import Crypto.Lol.Utils.PrettyPrint.Table
import Crypto.Lol.Types
import Crypto.Random.DRBG

import Data.Int
import Data.Proxy

--import HomomPRFBenches
import KHPRFBenches
import SHEBenches

infixr 9 **
data a ** b

type family Zq (a :: k) :: * where
  Zq (a ** b) = (Zq a, Zq b)
  Zq q = (ZqBasic q Int64)

bs :: [String]
bs = [
  "encrypt",
  "decrypt",
  "*",
  "addPublic",
  "mulPublic",
  "rescaleCT",
  "keySwitch",
  "tunnel"]

main :: IO ()
main = do
  let o = (defaultOpts "SHE"){benches=bs}
  benchGroups <- defaultBenches (Proxy::Proxy CT) (Proxy::Proxy TrivGad) (Proxy::Proxy HashDRBG)
  mapM_ (prettyBenches o) benchGroups

defaultBenches :: _ => Proxy t -> Proxy gad -> Proxy gen -> rnd [Benchmark]
defaultBenches pt pgad pgen  = sequence [
  benchGroup "SHE" $ ($ pt) <$>
    [sheBenches (Proxy::Proxy '(F16, F1024, Zq 8,  Zq 1017857)) pgen,
     sheBenches (Proxy::Proxy '(F16, F2048, Zq 16, Zq 1017857)) pgen],
  benchGroup "Dec" $ ($ pt) <$>
    [decBenches (Proxy::Proxy '(F16, F1024, Zq 8,  Zq 1017857)),
     decBenches (Proxy::Proxy '(F16, F2048, Zq 16, Zq 1017857))],
  benchGroup "Rescale" $ ($ pt) <$>
    [rescaleBenches (Proxy::Proxy '(F32, F2048,      Zq 16, Zq 1017857, Zq (1017857 ** 1032193))) pgad,
     rescaleBenches (Proxy::Proxy '(F32, F64*F9*F25, Zq 16, Zq 1008001, Zq (1008001 ** 1065601))) pgad],
  benchGroup "Rescale" $ ($ pt) <$>
    [tunnelBenches {- H0 -> H1 -} (Proxy::Proxy '(F128,
                                                  F128 * F7 * F13,
                                                  F64 * F7, F64 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)) pgad,
     tunnelBenches {- H1 -> H2 -} (Proxy::Proxy '(F64 * F7,
                                                  F64 * F7 * F13,
                                                  F32 * F7 * F13,
                                                  F32 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)) pgad,
     tunnelBenches {- H2 -> H3 -} (Proxy::Proxy '(F32 * F7 * F13,
                                                  F32 * F7 * F13,
                                                  F8 * F5 * F7 * F13,
                                                  F8 * F5 * F7 *F13,
                                                  Zq PP32,
                                                  Zq 3144961)) pgad,
     tunnelBenches {- H3 -> H4 -} (Proxy::Proxy '(F8 * F5 * F7 * F13,
                                                  F8 * F5 * F7 *F13,
                                                  F4 * F3 * F5 * F7 * F13,
                                                  F4 * F3 * F5 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)) pgad,
     tunnelBenches {- H4 -> H5 -} (Proxy::Proxy '(F4 * F3 * F5 * F7 * F13,
                                                  F4 * F3 * F5 * F7 *F13,
                                                  F9 * F5 * F7 * F13,
                                                  F9 * F5 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)) pgad]]

-- types for homomprf
type H0 = F128
type H1 = F64 * F7
type H2 = F32 * F7 * F13
type H3 = F8 * F5 * F7 * F13
type H4 = F4 * F3 * F5 * F7 * F13
type H5 = F9 * F5 * F7 * F13
type H0' = H0 * F7 * F13
type H1' = H1 * F13
type H2' = H2
type H3' = H3
type H4' = H4
type H5' = H5
type RngList = '[ '(H0,H0'), '(H1,H1'), '(H2,H2'), '(H3,H3'), '(H4,H4'), '(H5,H5') ]

type Zq (q :: k) = ZqBasic q Int64
-- three 24-bit moduli, enough to handle rounding for p=32 (depth-4 circuit at ~17 bits per mul)
type ZQ1 = Zq 18869761
type ZQ2 = (Zq 19393921, ZQ1)
type ZQ3 = (Zq 19918081, ZQ2)
-- a 31-bit modulus, for rounding off after the last four hops
type ZQ4 = (Zq 2149056001, ZQ3)
-- for rounding off after the first hop
type ZQ5 = (Zq 3144961, ZQ4)
type ZQ6 = (Zq 7338241, ZQ5)
type ZQSeq = '[ZQ6, ZQ5, ZQ4, ZQ3, ZQ2, ZQ1]

type ZP8 = Zq PP8

-- these need not be the same
type KSGad = BaseBGad 2
type PRFGad = BaseBGad 2

-- EAC: is there a simple way to parameterize the variance?
-- generates a secret key with scaled variance 1.0
instance (GenSKCtx t m' z Double) => Random (SK (Cyc t m' z)) where
  random = runRand $ genSK (1 :: Double)
  randomR = error "randomR not defined for SK"
