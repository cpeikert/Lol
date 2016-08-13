{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module BenchParams where

import Utils

import Crypto.Lol
import Crypto.Lol.Types
import Crypto.Random.DRBG

import Data.Singletons
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar ()




type Tensors = '[T]
type MRCombos =
  '[ '(M, R) ]

type T = CT
type M = F64*F9*F25 -- F9*F5*F7*F11 -- F64*F9*F25   --
type R = Zq 1065601 --34651 -- Zq 14401     --
type M' = M --F3*F5*F11

testParam :: Proxy '(T, M, R)
testParam = Proxy

testParam' :: Proxy '(T,M, R, HashDRBG)
testParam' = Proxy

twoIdxParam :: Proxy '(T, M', M, R)
twoIdxParam = Proxy

{-
type Tensors = '[CT,RT]
type MRCombos =
  '[ '(F1024, Zq 1051649),      -- 1024 / 512
     '(F2048, Zq 1054721),      -- 2048 / 1024
     '(F64 * F27, Zq 1048897),  -- 1728 / 576
     '(F64 * F81, Zq 1073089),  -- 5184 / 1728
     '(F64*F9*F25, Zq 1065601)  -- 14400 / 3840
    ]
-}

type MM'RCombos =
  '[ '(F8 * F91, F8 * F91 * F4, Zq 8737),
     '(F8 * F91, F8 * F91 * F5, Zq 14561),
     '(F128, F128 * F91, Zq 23297)
    ]

-- EAC: must be careful where we use Nub: apparently TypeRepStar doesn't work well with the Tensor constructors
type AllParams = ( '(,) <$> Tensors) <*> MRCombos
allParams :: Proxy AllParams
allParams = Proxy

type LiftParams = ( '(,) <$> Tensors) <*> MRCombos
liftParams :: Proxy LiftParams
liftParams = Proxy

type TwoIdxParams = ( '(,) <$> Tensors) <*> MM'RCombos
twoIdxParams :: Proxy TwoIdxParams
twoIdxParams = Proxy

type ErrorParams = ( '(,) <$> '[HashDRBG]) <*> LiftParams
errorParams :: Proxy ErrorParams
errorParams = Proxy

data Liftable :: TyFun (Factored, *) Bool -> *
type instance Apply Liftable '(m',r) = Int64 :== (LiftOf r)

data RemoveM :: TyFun (Factored, Factored, *) (Factored, *) -> *
type instance Apply RemoveM '(m,m',r) = '(m',r)
