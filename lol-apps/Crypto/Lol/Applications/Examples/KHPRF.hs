{-|
Module      : Crypto.Lol.Applications.Examples.KHPRF
Description : Example using KeyHomomorphicPRF.
Copyright   : (c) Chris Peikert, 2018
                  Bogdan Manga,  2018
License     : GPL-3
Maintainer  : bmanga@umich.edu
Stability   : experimental
Portability : POSIX

Example usage of 'Crypto.Lol.Applications.KeyHomomorphicPRF'.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Crypto.Lol.Applications.Examples.KHPRF (khprfMain) where

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Types

type SimpleTop = 'Intern ('Intern 'Leaf 'Leaf) 'Leaf
type M = F64
type N = 1
type Q = 257
type P = 2
type Rq t = Cyc t M (ZqBasic Q Int64)
type Rp t = Cyc t M (ZqBasic P Int64)
type Gad = BaseBGad 2

khprfMain :: forall t . _ => Proxy t -> IO ()
khprfMain _ = do
  key <- genKey
  params :: PRFParams N Gad (Rq t) <- genParams
  let t = singFBT :: SFBT SimpleTop
  let result :: [Matrix (Rp t)] =
        run $ sequence $ prfAmortized t params key <$> values
  print result
