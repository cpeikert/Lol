{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module HomomPRFMain where

import Control.DeepSeq
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import Crypto.Lol
import Crypto.Lol.Applications.HomomPRF
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Cyclotomic.Tensor.CPP as CPP

import HomomPRFParams

main :: IO ()
main = do
  let v = 1.0 :: Double
  sk <- genSK v
  (tHints, skout) <- tunnelHints sk
  rHints <- roundHints skout
  let hints = Hints tHints rHints :: EvalHints CPP.CT RngList Int64 ZP ZQ ZQSeq KSGad
  family :: PRFFamily PRFGad _ _ <- randomFamily 10 -- works on 10-bit input
  s <- getRandom
  let st = prfState family Nothing --initialize with input 0
  ct <- encrypt sk s
  let prf = homomPRFM ct
      xs = grayCode 3
      encprfs = flip runReader hints $ flip evalStateT st $ mapM prf xs
      decprfs = decrypt skout <$> encprfs
  decprfs `deepseq` return ()
