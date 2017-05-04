{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Crypto.Alchemy.Language.Tunnel where

import Control.Newtype
import Crypto.Lol (Cyc, Factored, Linear)
import GHC.Exts

-- | Symantics for leveled plaintext operations of some depth @d@.

class Tunnel expr where

  type PreTunnel expr (r :: Factored) b
  -- EAC: I tried using a instead of r, but in the PT2CT instance, a has a type family due to PNoise,
  -- and that is not allowed in associated type synonym parameters
  type TunnelCtxPT expr (e :: Factored) (r :: Factored) b :: Constraint

  tunnelPT :: (TunnelCtxPT expr e r b, a ~ PreTunnel expr r b,
               Newtype a (Cyc t r zp), Newtype b (Cyc t s zp))
           => Linear t zp e r s -> expr env (a -> b)