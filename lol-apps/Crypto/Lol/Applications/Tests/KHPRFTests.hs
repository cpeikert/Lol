{-|
Module      : Crypto.Lol.Applications.Tests.KHPRFTests
Description : Tests for KeyHomomorphicPRF.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tests for KeyHomomorphicPRF.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Tests.KHPRFTests (khprfTests) where

import Control.Applicative
import Control.Monad.Random hiding (lift)

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Cyclotomic.Cyc
import Crypto.Lol.Tests (showType, testIOWithoutGen, testGroup, Test)

import MathObj.Matrix

khprfTests :: forall t m zp zq gad . (_)
  => Proxy '(m,zp,zq,gad) -> Proxy t -> Test
khprfTests _ _ =
  let ptmrrg = Proxy::Proxy '(t,m,zp,zq,gad) in
  testGroup (showType ptmrrg) []
    testIOWithoutGen "PRF_3bits" (prop_keyHomom ptmrrg 3),
    testIOWithoutGen "PRF_5bits" (prop_keyHomom ptmrrg 5)]

-- TODO: Fix the uncycPow here

-- +/-1 in every coefficient of the rounding basis
prop_keyHomom :: forall t m zp zq gad . _ => Proxy '(t,m,zp,zq,gad) -> Int -> IO Bool
prop_keyHomom _ size = do
  family :: PRFFamily gad (Cyc t m zq) (Cyc t m zp) <- randomFamily size
  s1 <- getRandom
  s2 <- getRandom
  x <- ((`mod` (2^size)) . abs) <$> getRandom
  let s3 = s1+s2
      state = prfState family Nothing
      prf1 = ringPRF s1 x state
      prf2 = ringPRF s2 x state
      prf3 = ringPRF s3 x state
      prf3' = prf1+prf2 :: Matrix (Cyc t m zp)
      a = uncycPow <$> prf3
      b = uncycPow <$> prf3'
      c = concat $ rows $ a - b
      c' = map (maximum . fmapPow abs . lift) c
  return $ maximum c' <= 1
