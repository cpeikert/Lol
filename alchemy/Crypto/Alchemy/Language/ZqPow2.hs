{-|

  \( \def\Z{\mathbb{Z}} \)

-}

{-# LANGUAGE DataKinds #-}

module ZqPow2 where

import Crypto.Lol       (Pos (..), Prime2, PrimePower (PP))
import Crypto.Lol.Types (ZqBasic)

type Pow2 e = 'PP '(Prime2, e)

-- | Symantics for division-by-2 of argument and modulus.

class Div2ZqPow2 expr where
  -- | Divide the argument and its modulus by 2, when the argument is
  -- known to be even (otherwise behavior may be undefined).
  div2ZqPow2 :: expr (ZqBasic (Pow2 ('S e)) i -> ZqBasic (Pow2 e) i)

-- | Symantics for rescaling on a power-of-two modulus.

class RescaleZqPow2 expr where
  -- | Rescale (round) the argument from \( \Z_{2^e} \) to \( \Z_2 \).
  rescaleZqPow2 :: expr (ZqBasic (Pow2 e) i -> ZqBasic (Pow2 'O) i)
