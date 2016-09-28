{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses,
             RebindableSyntax, ScopedTypeVariables,
             UndecidableInstances #-}

module GenArgs.Zq where

import GenArgs

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol

data LiftedMod r where
  LMod :: (ToInteger (ModRep r)) => ModRep r -> LiftedMod r

data LiftedInvertible r where
  LInv :: (ToInteger (ModRep r)) => ModRep r -> LiftedInvertible r

newtype Invertible r = Invertible r

instance (MonadRandom rnd, Mod r, Random (ModRep r), ToInteger (ModRep r))
  => Generatable rnd (LiftedMod r) where
  genArg =
    let q = proxy modulus (Proxy::Proxy r)
    in LMod <$> getRandomR (0,q-1)

instance (MonadRandom rnd, Mod r, Random (ModRep r), PID (ModRep r), ToInteger (ModRep r))
  => Generatable rnd (LiftedInvertible r) where
  genArg =
    let q = proxy modulus (Proxy::Proxy r)
        go = do
          x <- getRandomR (1,q-1)
          if gcd x q == 1
          then return $ LInv x
          else go
    in go

instance (MonadRandom rnd, Generatable rnd (LiftedInvertible r), Ring r)
  => Generatable rnd (Invertible r) where
  genArg = do
    (LInv x) :: LiftedInvertible r <- genArg
    return $ Invertible $ fromIntegral x