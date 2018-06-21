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

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Crypto.Lol.Applications.Examples.KHPRF
(khprfMain
) where

import Crypto.Lol hiding (replicate)
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Types

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

type Top = 'Intern ('Intern 'Leaf 'Leaf) 'Leaf
type N = 'O
type Q = 257
type P = 2
type Rq = ZqBasic Q Int64
type Rp = ZqBasic P Int64
type Gad = BaseBGad 2

khprfMain :: IO ()
khprfMain = do
    key    <- genKey
    params <- genParams
    let tree :: FBT Top N Gad Rq = defaultFBT params
    let result :: [Matrix Rp] = flip evalState tree $ flip runReaderT params $
                                sequence $ prf key <$> take 8 [replicate False ..]
    print result
