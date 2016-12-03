{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HomomPRFParams (RngList, Zq, ZQSeq, ZP, ZQ, KSGad, PRFGad) where

import Crypto.Lol
import Crypto.Lol.Types

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
type ZQ4 = (Zq 25159681, ZQ3)
-- a 31-bit modulus, for rounding off after the last four hops
type ZQ5 = (Zq 2149056001, ZQ4)
-- for rounding off after the first hop
type ZQ6 = (Zq 3144961, ZQ5)
type ZQ7 = (Zq 7338241, ZQ6)
type ZQSeq = '[ZQ7, ZQ6, ZQ5, ZQ4, ZQ3, ZQ2, ZQ1]

type ZP = Zq PP32
type ZQ = ZQ6 -- if p=2^k, choose ZQ[k+1]

-- these need not be the same
type KSGad = BaseBGad 2
type PRFGad = BaseBGad 2