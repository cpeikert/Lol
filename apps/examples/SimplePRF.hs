{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random hiding (fromList)
import Control.Monad.State hiding (state)

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types

import MathObj.Matrix hiding (zipWith)

type Zq q = ZqBasic q Int64
type Cyclo m q = Cyc CT m (Zq q)
type Gad = BaseBGad 2

main :: IO ()
main = do
  family :: PRFFamily Gad (Cyclo F32 257) (Cyclo F32 32) <- randomFamily 10 -- works on 10-bit input
  s <- getRandom
  let state = prfState family Nothing --initialize with input 0
      prf = ringPRFM s
      res = map rows $ flip evalState state $ mapM prf [0,1,3,2,6,7,5,4] -- grey code
  res `deepseq` print "done"

main2 :: IO ()
main2 = do
  let n = 3 -- 3 rows/matrix
      k = 10 -- 10 bit input
      t = balancedTree k
      gadLen = 9
  a0 <- fromList n (n*gadLen) <$> take (gadLen*n*n) <$> getRandoms
  a1 <- fromList n (n*gadLen) <$> take (gadLen*n) <$> getRandoms
  let family = makeFamily a0 a1 t :: PRFFamily Gad (Zq 257) (Zq 32)
  s <- fromList 1 n <$> take n <$> getRandoms
  let state = prfState family Nothing -- initialize with input 0
      prf x = latticePRFM s x
      res = map rows $ flip evalState state $ mapM prf [0,1,3,2,6,7,5,4] -- grey code
  print res
