{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UndecidableInstances #-}

module Crypto.Alchemy.Interpreter.ErrorCT where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Writer
import Data.Maybe
import Data.Typeable
import Crypto.Alchemy.Interpreter.Compiler.Environment

import Crypto.Alchemy.Interpreter.Eval
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.SHE
import qualified Crypto.Lol.Applications.SymmSHE as SHE
import           Crypto.Lol.Applications.SymmSHE (CT, SK, ToSDCtx, errorTermUnrestricted)

-- we might like to enforce that E is *somewhere* at the bottom of the resulting expression,
-- not that it necessarily *is* E. This is analogous to a monad transformer class...

{-
instance Lambda ErrorCT where
  lam f = ECT (\s ->
    let (SCT b) = f $ ECT $ const s
    in "\\" ++ x ++ " -> " ++ (b $ i+1)
  app (ECT sf f) (ECT sa a) = ECT (sf . sa) $ app f a

-}

--getErrors :: P2CState -> ErrorCT (WriterT [String] (State P2CState)) () a -> (E () a, [String])
--getErrors st (ECT a) = evalState (runWriterT a) st

newtype ErrorCT e a = ECT (e -> Writer [String] a) deriving (Functor)




instance Lambda ErrorCT where
  -- | Abstraction.
  lam  :: expr (e,a) b -> expr e (a -> b)
  lam (ECT a) = ECT $ \e ->

  -- | Application.
  ($:) :: expr e (a -> b) -> expr e a -> expr e b

-- | Let-sharing.
let_ :: Lambda expr => expr e a -> expr (e,a) b -> expr e b
let_ a f = lam f $: a

-- | Symantics for de Bruijn variables.

class DB expr a where
  -- | The zero'th (most recently bound) variable.
  v0 :: expr (b,a) a

  -- | The next-most-recently-bound variable from the given one.
  s  :: expr e a -> expr (e,x) a



















-- | Metacircular ciphertext symantics.
instance (MonadState P2CState m, MonadWriter [String] m) => SHE (ErrorCT m) where

  type ModSwitchPTCtx   (ErrorCT m) ct zp'     = (ModSwitchPTCtx E ct zp')
  type RescaleLinearCtx (ErrorCT m) ct zq'     = (RescaleLinearCtx E ct zq')
  type AddPublicCtx     (ErrorCT m) ct         = (AddPublicCtx E ct)
  type MulPublicCtx     (ErrorCT m) ct         = (MulPublicCtx E ct)
  type KeySwitchQuadCtx (ErrorCT m) ct zq' gad = (KeySwitchQuadCtx E ct zq' gad)
  type TunnelCtx        (ErrorCT m) t e r s e' r' s' zp zq gad =
    (TunnelCtx    E t e r s e' r' s' zp zq gad,
     LogCtx t r' (LiftOf zp) zp zq, LogCtx t s' (LiftOf zp) zp zq)
{-
  modSwitchPT (ECT a) = ECT $ modSwitchPT <$> a
  rescaleLinear (ECT a) = ECT $ rescaleLinear <$> a
  addPublic b (ECT a) = ECT $ addPublic b <$> a
  mulPublic b (ECT a) = ECT $ mulPublic b <$> a
  keySwitchQuad h (ECT a) = ECT $ keySwitchQuad h <$> a
-}
  tunnel f (ECT a) = ECT $ \e ->
    let (a', w) = runWriter a


    in \e ->
         let b = a' e



    a' <- a e
    logError "Pre-tunnel" a'
    let b = unE (tunnel f (E $ const a')) e
    logError "Post-tunnel" b
    return $ b



errRatio :: forall m zp t m' zq z .
  (Reduce z zq, Lift' zq, CElt t z, ToSDCtx t m' zp zq, Mod zq, Absolute (LiftOf zq),
   Ord (LiftOf zq), ToInteger (LiftOf zq))
  => SHE.CT m zp (Cyc t m' zq) -> SK (Cyc t m' z) -> Double
errRatio ct sk =
  (fromIntegral $ maximum $ fmap abs $ errorTermUnrestricted sk ct) /
  (fromIntegral $ proxy modulus (Proxy::Proxy zq))

type LogCtx t m' z zp zq = (Reduce z zq, Lift' zq, CElt t z, ToSDCtx t m' zp zq, Mod zq, Absolute (LiftOf zq),
   Ord (LiftOf zq), ToInteger (LiftOf zq),
   Typeable t, Typeable m', Typeable z)

logError :: forall mon t m m' z zp zq .
  (z ~ LiftOf zp, MonadState P2CState mon,
   MonadWriter [String] mon, LogCtx t m' z zp zq)
  => String -> CT m zp (Cyc t m' zq) -> mon ()
logError op a = do
  (sk :: SK (Cyc t m' z)) <- fromJust <$> keyLookup
  let err = errRatio a sk
  tell $ [op ++ ": " ++ (show err)]

