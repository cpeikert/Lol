{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module HomomPRFBenches where

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
