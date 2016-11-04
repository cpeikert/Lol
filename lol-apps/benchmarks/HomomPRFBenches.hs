{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Control.DeepSeq
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import Crypto.Lol
import Crypto.Lol.Applications.HomomPRF
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Types hiding (CT)
import qualified Crypto.Lol.Types as Lol (CT)

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


benchHomomPRF :: forall t m zq (zp :: *) (gad :: *) . (_)
  => Int -> (Int -> FullBinTree) -> [Int] -> As 1 gad (Cyc t m zq) -> CT r zp (Cyc t r' zq) -> Bench '(t,m,zq,zp,gad)
benchHomomPRF size t xs (As a0 a1) ct


main :: IO ()
main = do
  let v = 1.0
  sk <- genSK v
  (tHints, skout) <- tunnelHints v sk
  rHints <- roundHints skout
  let hints = Hints tHints rHints :: EvalHints Lol.CT RngList Int64 ZP8 ZQ4 ZQSeq KSGad Double
      family = makeFamily a0 a1 (t size) :: PRFFamily gad (Cyc t m zq) (Cyc t m zp)
      st = prfState family Nothing --initialize with input 0
      encprfs = flip runReader hints $ flip evalStateT st $ mapM (homomPRFM ct) xs
      decprfs = decrypt skout <$> encprfs
  decprfs `deepseq` return ()
