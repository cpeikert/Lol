{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Params.LolParams where

import Utils

import Crypto.Lol
import Crypto.Lol.Types
import Crypto.Random.DRBG

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


type MM'RCombos =
  '[ '(F8 * F91, F8 * F91 * F4, Zq 8737),
     '(F8 * F91, F8 * F91 * F5, Zq 14561),
     '(F128, F128 * F91, Zq 23297)
    ]
-}
