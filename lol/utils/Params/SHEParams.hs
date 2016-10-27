{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Params.SHEParams where

import Utils

import Crypto.Lol
import Crypto.Lol.Types
import Crypto.Random.DRBG

type T = CT
type M = F64*F9*F25
type R = Zq  1065601
type R' = Zq 1108801
type M' = F16

type MM'RCombos =
  '[ '(F8 * F91, F8 * F91 * F4, Zq 8737),
     '(F8 * F91, F8 * F91 * F5, Zq 14561),
     '(F128, F128 * F91, Zq 23297)
    ]



enc_param :: Proxy '(T, M', M, Zq 2, R, HashDRBG)
enc_param = Proxy

dec_param :: Proxy '(T, M', M, Zq 2, R)
dec_param = Proxy

rescale_param :: Proxy '(T, M', M, Zq 2, R, R')
rescale_param = Proxy

ksw_param :: Proxy '(T, M', M, Zq 2, R, R', TrivGad)
ksw_param = Proxy

-- '(t,r,r',s,s',zp,zq,gad)
tunn_param :: Proxy '(T, 
                      F4 * F3 * F5 * F7 * F13, F4 * F3 * F5 * F7 *F13, F9 * F5 * F7 * F13, F9 * F5 * F7 * F13,
                      Zq PP32, Zq 3144961, TrivGad)
tunn_param = Proxy
{-
type TunnRings = '[
  {- H0 -> H1 -} '(F128, F128 * F7 * F13, F64 * F7, F64 * F7 * F13),
  {- H1 -> H2 -} '(F64 * F7, F64 * F7 * F13, F32 * F7 * F13, F32 * F7 * F13),
  {- H2 -> H3 -} '(F32 * F7 * F13, F32 * F7 * F13, F8 * F5 * F7 * F13, F8 * F5 * F7 *F13),
  {- H3 -> H4 -} '(F8 * F5 * F7 * F13, F8 * F5 * F7 *F13, F4 * F3 * F5 * F7 * F13, F4 * F3 * F5 * F7 * F13),
  {- H4 -> H5 -} '(F4 * F3 * F5 * F7 * F13, F4 * F3 * F5 * F7 *F13, F9 * F5 * F7 * F13, F9 * F5 * F7 * F13)
    ]

-}

