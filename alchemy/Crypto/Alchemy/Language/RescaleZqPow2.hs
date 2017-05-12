{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-|

  \( \def\Z{\mathbb{Z}} \)

-}

module Crypto.Alchemy.Language.RescaleZqPow2 where

import Crypto.Lol (Pos)

-- | Symantics for rescaling the integers modulo a power of two.

class RescaleZqPow2 expr (k::Pos) z2 where
  -- | The type corresponding to \( \Z_{2^k} \).  (The type should
  -- determine the exponent, hence the partially injectivity.)
  type PreRescaleZqPow2 expr (k::Pos) z2 = z2k | z2k -> k

  -- | Rescale (round) the argument from \( \Z_{2^k} \) to \( \Z_2 \).
  rescaleZqPow2_ :: expr e (PreRescaleZqPow2 expr k z2 -> z2)
