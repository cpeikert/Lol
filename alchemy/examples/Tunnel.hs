{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tunnel where

import Algebra.Additive as Additive (C(..))
import qualified Algebra.Ring as Ring (C(..))
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Random
import Control.Monad.Writer
import Data.Type.Natural

import Common
import LinearDec2CRT
import Crypto.Alchemy.MonadAccumulator
--import Crypto.Alchemy.Interpreter.DedupRescale
import Crypto.Alchemy.Interpreter.Depth
import Crypto.Alchemy.Interpreter.Dup
import Crypto.Alchemy.Interpreter.ErrorRateWriter
import Crypto.Alchemy.Interpreter.Eval
import Crypto.Alchemy.Interpreter.KeysHints
import Crypto.Alchemy.Interpreter.Params
import Crypto.Alchemy.Interpreter.Print
import Crypto.Alchemy.Interpreter.PT2CT
import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Interpreter.Size

import Crypto.Lol hiding (Pos(..))
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Types

type Gad = BaseBGad 2
type Zqs = '[ Zq $(mkTLNatNat 537264001),
              Zq $(mkTLNatNat 539360641),
              Zq $(mkTLNatNat 539884801),
              Zq $(mkTLNatNat 540933121),
              Zq $(mkTLNatNat 541457281) ] -- good moduli, ~ 30 bits

main :: IO ()
main = do
  let (exp1a, exp2a) = dup $ linear2 @CT @H0 @H1 @H2 @(Zq PP8) @Identity Proxy

  -- example with rescale de-duplication when tunneling
  -- print the unapplied PT function
  putStrLn $ pprint exp1a
  putStrLn $ show $ eval exp2a 2


  let ptexpr = linear2 @CT @H0 @H1 @H2 @(Zq PP8) @(PNoise 'Z) Proxy :: PT2CT' RngList Zqs Gad _
  --let ptexpr = linear5 @CT @'[H0,H1,H2,H3,H4,H5] @(Zq PP8) @(PNoise 'Z) Proxy
  putStrLn $ "PT expression params:\n" ++ (params ptexpr $ linear2 @_ @_ @H1 Proxy)

  pt1 <- getRandom

  -- compile the up-applied function to CT, then print it out
  evalKeysHints 8.0 $ do
    y <- argToReader (pt2ct
         @RngList
         @Zqs
         @Gad
         @Int64)
         (linear2 @CT @H0 @H1 @H2 @(Zq PP8) @(PNoise 'Z) Proxy)
         --(tunn5 @CT @'[H0,H1,H2,H3,H4,H5] @(Zq PP8) @(PNoise 'Z) Proxy)
    -- compile once, interpret with multiple ctexprs!!
    let (z1,z2) = dup y
        (w1,w2) = dup z1
    liftIO $ putStrLn $ pprint w1
    liftIO $ putStrLn $ params w1 w2
    arg1 <- argToReader encrypt pt1

    z2' <- readerToAccumulator $ writeErrorRates @Int64 @() z2
    let (_,errors) = runWriter $ eval z2' $ return arg1
    liftIO $ print $ "Error rates: " ++ show errors
    --liftIO $ putStrLn $ pprint $ dedupRescale z2

type RngList = '[ '(H0,H0'), '(H1,H1'), '(H2,H2')] -- , '(H3,H3'), '(H4,H4'), '(H5,H5') ]
