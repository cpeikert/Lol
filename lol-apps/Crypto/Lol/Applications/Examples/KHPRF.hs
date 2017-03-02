{-|
Module      : Crypto.Lol.Applications.Examples.KHPRF
Description : Example using KeyHomomorphicPRF.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example using KeyHomomorphicPRF.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Examples.KHPRF (khprfRingMain, khprfLatticeMain) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random hiding (fromList)
import Control.Monad.State hiding (state)

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Types

import MathObj.Matrix hiding (zipWith)

type Zq q = ZqBasic q Int64
type Cyclo t m q = Cyc t m (Zq q)
type Gad = BaseBGad 2
type M = F128

-- | Driver for a key-homomorphic PRF over rings.
khprfRingMain :: forall t . (_) => Proxy t -> IO ()
khprfRingMain _ = do
  family :: PRFFamily Gad (Cyclo t M 257) (Cyclo t M 32) <- randomFamily 10 -- works on 10-bit input
  s <- getRandom                                                            -- prf seed
  let state = prfState family Nothing                                       -- initialize with input 0
      prf = ringPRFM s
      xs = grayCode 3
      res = map rows $ flip evalState state $ mapM prf xs
  res `deepseq` print "done"

-- | Driver for a key-homomorphic PRF over lattices.
khprfLatticeMain :: IO ()
khprfLatticeMain = do
  let n = 3 -- 3 rows/matrix
      k = 10 -- 10 bit input
      t = balancedTree k
      gadLen = 9
  a0 <- fromList n (n*gadLen) <$> take (gadLen*n*n) <$> getRandoms
  a1 <- fromList n (n*gadLen) <$> take (gadLen*n*n) <$> getRandoms
  let family = makeFamily a0 a1 t :: PRFFamily Gad (Zq 257) (Zq 32)
  s <- fromList 1 n <$> take n <$> getRandoms
  let state = prfState family Nothing -- initialize with input 0
      prf x = latticePRFM s x
      xs = grayCode 3
      res = map rows $ flip evalState state $ mapM prf xs
  print res
