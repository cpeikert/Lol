{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

import Crypto.Lol.Benchmarks
import Crypto.Lol.Factored
import Crypto.Lol.Gadget
import Crypto.Lol.Utils.PrettyPrint
import Crypto.Lol.Types
import Crypto.Random.DRBG

import Data.Int
import Data.Proxy

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
  "tunnel"
    ]

main :: IO ()
main = do
  let opts = defaultOpts {benches=bs,layers=["SHE"]}
  b1 <- defaultBenches (Proxy::Proxy CT) (Proxy::Proxy TrivGad) (Proxy::Proxy HashDRBG)
  let benchGroups = [b1]
  mapM_ (prettyBenches opts) benchGroups

defaultBenches :: _ => Proxy t -> Proxy gad -> Proxy gen -> rnd Benchmark
defaultBenches pt pgad pgen  = benchGroup "SHE" $ concat $ ($ pt) <$>
  [sheBenches (Proxy::Proxy '(F16, F1024, Zq 8,  Zq 1017857)) pgen,
   sheBenches (Proxy::Proxy '(F16, F2048, Zq 16, Zq 1017857)) pgen,
   decBenches (Proxy::Proxy '(F16, F1024, Zq 8,  Zq 1017857)),
   decBenches (Proxy::Proxy '(F16, F2048, Zq 16, Zq 1017857)),
   rescaleBenches (Proxy::Proxy '(F32, F2048,      Zq 16, Zq 1017857, Zq (1017857 ** 1032193))) pgad,
   rescaleBenches (Proxy::Proxy '(F32, F64*F9*F25, Zq 16, Zq 1008001, Zq (1008001 ** 1065601))) pgad,
   tunnelBenches {- H0 -> H1 -} (Proxy::Proxy '(F128,
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
                                                Zq 3144961)) pgad
   ]
