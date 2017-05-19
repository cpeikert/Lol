{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Common where

import qualified Algebra.Additive as Additive (C(..))
import qualified Algebra.Ring as Ring (C(..))

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Crypto.Alchemy.Interpreter.KeysHints

import Crypto.Alchemy.Interpreter.Print
import Crypto.Alchemy.Interpreter.PT2CT
import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Lol
import Crypto.Lol.Types

-- a concrete Z_2^e data type
type Z2E e = ZqBasic ('PP '(Prime2, e)) Int64

-- EAC: these instances need a home
deriving instance (Additive a) => Additive.C (Identity a)
deriving instance (Ring a) => Ring.C (Identity a)

-- EAC: This is a convenient function, but it needs a home.
argToReader :: (MonadReader v mon) => (v -> a -> mon b) -> a -> mon b
argToReader f a = flip f a =<< ask


type Zq q = ZqBasic q Int64

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

type PTRngs = '[H0,H1,H2,H3,H4,H5]

type CTRngs = '[ '(H0,H0'), '(H1,H1'), '(H2,H2'), '(H3,H3'), '(H4,H4'), '(H5,H5') ]


type PT2CT' m'map zqs gad a = PT2CT m'map zqs gad Int64 P (StateT Keys (StateT Hints (ReaderT Double IO))) () a