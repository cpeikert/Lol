{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-|

  \( \def\Z{\mathbb{Z}} \)

-}

{-# LANGUAGE DataKinds #-}

module Crypto.Alchemy.Language.Zq where

import Crypto.Lol       (Pos (..), Prime2, PrimePower (PP))
import Crypto.Lol.Types (ZqBasic)

type Pow2 e = 'PP '(Prime2, e)

-- | Symantics for division-by-2 of both the value and its modulus.

class Div2Zq expr a where
  type family PreDiv expr a

  div2Zq_ :: expr (PreDiv expr a -> a)


-- | Symantics for rescaling on a power-of-two modulus.

class RescaleZqPow2 expr where
  -- | Rescale (round) the argument from \( \Z_{2^e} \) to \( \Z_2 \).
  rescaleZqPow2_ :: expr (ZqBasic (Pow2 e) i -> ZqBasic (Pow2 'O) i)
