{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module HomomPRFBenches where

import Control.Monad.Random hiding (fromList)
import Control.Monad.Reader
import Control.Monad.State

import Crypto.Lol
import Crypto.Lol.Applications.HomomPRF
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Benchmarks hiding (bench)
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types

import Data.Promotion.Prelude.List

import Criterion
import MathObj.Matrix
import HomomPRFParams

benchHomomPRF :: forall t m zp rp (prfgad :: *) r r' s s' e rnd .
  (m ~ Fst (Head RngList), zp ~ ZP, rp ~ Cyc t m zp,
   CElt t zp, Decompose prfgad zp, MonadRandom rnd,
   MulPublicCtx t r r' ZP ZQ,
   MultiTunnelCtx RngList r r' s s' t Int64 ZP ZQ KSGad ZQSeq,
   PTRound t s s' e ZP (ZqDown ZQ ZQSeq) Int64 KSGad ZQSeq,
   TElt t (ZqBasic PP2 Int64), TElt t (Last ZQSeq))
  => Int -> (Int -> FullBinTree) -> [Int] -> Proxy t -> Proxy prfgad -> rnd Benchmark
benchHomomPRF size t xs _ _ = benchGroup "HomomPRF" $ (:[]) $ do
  let v = 1.0 :: Double
  sk <- genSK v
  (tHints, skout) <- tunnelHints sk
  rHints <- roundHints skout
  let gadLen = length $ untag (gadget :: Tagged prfgad [rp])
  a0 <- fromList 1 gadLen <$> take gadLen <$> getRandoms
  a1 <- fromList 1 gadLen <$> take gadLen <$> getRandoms
  let hints = Hints tHints rHints :: EvalHints t RngList Int64 ZP ZQ ZQSeq KSGad
      family = makeFamily a0 a1 (t size) :: PRFFamily prfgad _ _
  s <- getRandom
  ct <- encrypt sk s
  return (bench "homomprf" $ nf
    (let st = prfState family Nothing --initialize with input 0
         encprfs = flip runReader hints . flip evalStateT st . mapM (homomPRFM ct)
     in map (decrypt skout) . encprfs) xs :: Benchmark)
