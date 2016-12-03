{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module KHPRFMain where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random hiding (fromList)
import Control.Monad.State hiding (state)

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Types

import MathObj.Matrix hiding (zipWith)

type Zq q = ZqBasic q Int64
type Cyclo m q = Cyc CT m (Zq q)
type Gad = BaseBGad 2
type M = F128

type Cyclo' q = Cyclo M q

main :: IO ()
main = do
  family :: PRFFamily Gad (Cyclo' 257) (Cyclo M 32) <- randomFamily 10 -- works on 10-bit input
  s <- getRandom                                                            -- prf seed
  let state = prfState family Nothing                                       -- initialize with input 0
      prf = ringPRFM s
      xs = grayCode 3
      res = map rows $ flip evalState state $ mapM prf xs
  res `deepseq` print "done"

main2 :: IO ()
main2 = do
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
