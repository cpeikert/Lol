{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Alchemy.Interpreter.Eval ( E, eval) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Data.Tuple

import Algebra.Additive as Additive
import Algebra.Ring     as Ring
import NumericPrelude

import Crypto.Alchemy.Interpreter.Compiler.Environment
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.SHE

import Crypto.Lol (Cyc, RescaleCyc, LiftOf, CElt, Reduce, Lift', Mod, ToInteger, Absolute, Proxy)
import qualified Crypto.Lol.Applications.SymmSHE as SHE
import           Crypto.Lol.Applications.SymmSHE (CT, SK, MulPublicCtx, AddPublicCtx, ToSDCtx, ModSwitchPTCtx)

-- | Metacircular evaluator.
newtype E e a = E { unE :: e -> a } deriving (Functor, Applicative)

-- | Evaluate a closed expression (i.e., one not having any unbound
-- variables)
eval :: E () a -> a
eval = flip unE ()

instance Lambda E where
  lam f  = E $ curry $ unE f
  ($:) = (<*>)

instance DB E a where
  v0  = E snd
  s a = E $ unE a . fst

instance (Additive.C a) => Add E a where
  x +: y = (+) <$> x <*> y
  negate' x = negate <$> x

instance (Additive.C a) => AddLit E a where
  addLit x y = (x +) <$> y

instance (Ring.C a) => Mul E a where
  type PreMul E a = a
  x *: y = (*) <$> x <*> y

instance (Ring.C a) => MulLit E a where
  mulLit x y = (x *) <$> y

instance SHE E where

  type ModSwitchCtx E (CT m zp (Cyc t m' zq)) zp'     = (ModSwitchPTCtx t m' zp zp' zq)
  type RescaleCtx   E (CT m zp (Cyc t m' zq)) zq'     = (RescaleCyc (Cyc t) zq' zq, ToSDCtx t m' zp zq')
  type AddPubCtx    E (CT m zp (Cyc t m' zq))         = (AddPublicCtx t m m' zp zq)
  type MulPubCtx    E (CT m zp (Cyc t m' zq))         = (MulPublicCtx t m m' zp zq)
  type KeySwitchCtx E (CT m zp (Cyc t m' zq)) zq' gad = (SHE.KeySwitchCtx gad t m' zp zq zq')
  type TunnelCtx    E t e r s e' r' s' zp zq gad      = (SHE.TunnelCtx t r s e' r' s' zp zq gad)

  modSwitchPT     = fmap SHE.modSwitchPT
  rescaleLinearCT = fmap SHE.rescaleLinearCT
  addPublic       = fmap . SHE.addPublic
  mulPublic       = fmap . SHE.mulPublic
  keySwitchQuad   = fmap . SHE.keySwitchQuadCirc
  tunnel          = fmap . SHE.tunnelCT




getErrors :: P2CState -> ErrorCT (WriterT [String] (Reader P2CState)) () a -> (E () a, [String])
getErrors st (ECT a) = runReader (runWriterT a) st

newtype ErrorCT m e a = ECT (m (E e a)) deriving (Functor)

-- | Metacircular ciphertext symantics.
instance (MonadReader P2CState m, MonadWriter [String] m) => SHE (ErrorCT m) where

  type ModSwitchCtx (ErrorCT m) ct zp'     = (ModSwitchCtx E ct zp')
  type RescaleCtx   (ErrorCT m) ct zq'     = (RescaleCtx   E ct zq')
  type AddPubCtx    (ErrorCT m) ct         = (AddPubCtx    E ct)
  type MulPubCtx    (ErrorCT m) ct         = (MulPubCtx    E ct)
  type KeySwitchCtx (ErrorCT m) ct zq' gad = (KeySwitchCtx E ct zq' gad)
  type TunnelCtx    (ErrorCT m) t e r s e' r' s' zp zq gad =
    (TunnelCtx    E t e r s e' r' s' zp zq gad,
     Reduce z zq, Lift' zq, CElt t z, ToSDCtx t m' zp zq, z ~ LiftOf zp)

  modSwitchPT (ECT a) = ECT $ modSwitchPT <$> a
  rescaleLinearCT (ECT a) = ECT $ rescaleLinearCT <$> a
  addPublic b (ECT a) = ECT $ addPublic b <$> a
  mulPublic b (ECT a) = ECT $ mulPublic b <$> a
  keySwitchQuad h (ECT a) = ECT $ keySwitchQuad h <$> a
  tunnel        f (ECT a) = ECT $ do
    logError "Pre-tunnel" a
    let b = tunnel f <$> a
    logError "Post-tunnel" b
    b



errRatio :: forall m zp t m' zq z .
  (Reduce z zq, Lift' zq, CElt t z, ToSDCtx t m' zp zq, Mod zq, Absolute (LiftOf zq),
   Ord (LiftOf zq), ToInteger (LiftOf zq))
  => SHE.CT m zp (Cyc t m' zq) -> SK (Cyc t m' z) -> Double
errRatio ct sk =
  (fromIntegral $ maximum $ fmap abs $ errorTermUnrestricted sk ct) /
  (fromIntegral $ proxy modulus (Proxy::Proxy zq))

-- in general, this is unsafe.
stToReader :: (MonadReader a n) => State a b -> n b
stToReader st = lift $ ReaderT $ evalState st

type LogCtx t m m' z zp zq

logError :: (MonadReader P2CState mon, MonadWriter [String] mon,
             Reduce z zq, Lift' zq, CElt t z, ToSDCtx t m' zp zq, z ~ LiftOf zp)
  => String -> mon (E e (CT m zp (Cyc t m' zq))) -> mon ()
logError op a = do
  sk <- stToReader $ fromJust <$> keyLookup
  a' <- a
  let err = errRatio (eval a') sk
  tell $ op ++ ": " ++ show err

