{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-|

  \( \def\Z{\mathbb{Z}} \)

-}

module Crypto.Alchemy.Language.RescaleZqPow2 where

import Crypto.Lol (Pos, Tagged)
import Crypto.Alchemy.Language.Lambda

-- | Symantics for rescaling the integers modulo a power of two.

class RescaleZqPow2 expr (k::Pos) z2 where

  type PreRescaleZqPow2 expr (k::Pos) z2

  -- | Rescale (round) the argument from \( \Z_{2^k} \) to \( \Z_2 \).
  rescaleZqPow2_ :: Tagged k (expr e (PreRescaleZqPow2 expr k z2 -> z2))

rescaleZqPow2 :: (Lambda expr, RescaleZqPow2 expr k z2)
  => Tagged k (expr e (PreRescaleZqPow2 expr k z2) -> expr e z2)
rescaleZqPow2 = ($:) <$> rescaleZqPow2_
