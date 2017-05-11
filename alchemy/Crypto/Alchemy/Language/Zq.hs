{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-|

  \( \def\Z{\mathbb{Z}} \)

-}


module Crypto.Alchemy.Language.Zq where

import Crypto.Lol (Pos)

-- | Symantics for division-by-2 of both the value and its modulus.

class Div2Zq expr zq where
  type PreDiv2Zq expr zq

  div2Zq_ :: expr (PreDiv2Zq expr zq -> zq)


-- | Symantics for rescaling the integers modulo a power of two.

class RescaleZqPow2 expr z2 where
  -- | The type corresponding to \( \Z_{2^k} \).  (The type should
  -- determine the exponent, hence the partially injectivity.)
  type PreRescaleZqPow2 expr (k::Pos) z2 = z2k | z2k -> k

  -- | Rescale (round) the argument from \( \Z_{2^k} \) to \( \Z_2 \).
  rescaleZqPow2_ :: expr e (PreRescaleZqPow2 expr k z2 -> z2)
