{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-|

  \( \def\Z{\mathbb{Z}} \)

-}

{-# LANGUAGE DataKinds #-}

module Crypto.Alchemy.Language.ZqPow2 where

import Crypto.Lol       (Pos (..), Prime2, PrimePower (PP))
import Crypto.Lol.Types (ZqBasic)

type Pow2 e = 'PP '(Prime2, e)

-- | Symantics for division-by-2 of argument and modulus.

class DivZq expr a where
  type family PreDiv expr a

  divZq :: expr (PreDiv expr a -> a)


-- | Symantics for rescaling on a power-of-two modulus.

class RescaleZqPow2 expr where
  -- | Rescale (round) the argument from \( \Z_{2^e} \) to \( \Z_2 \).
  rescaleZqPow2 :: expr (ZqBasic (Pow2 e) i -> ZqBasic (Pow2 'O) i)


-- mapCRTSlots (rescaleTree rescaleZqPow2) :: (Arithmetic expr) => expr (cyc8 -> cyc2)
