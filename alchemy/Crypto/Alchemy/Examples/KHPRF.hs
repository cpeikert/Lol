{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Crypto.Alchemy.Examples.KHPRF where

import Crypto.Lol
import Crypto.Lol.Types

import Crypto.Alchemy.Interpreter.MapCRTSlots
import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Interpreter.RescaleToTree
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.RescaleZqPow2
import Crypto.Alchemy.Language.TunnelCyc

-- a concrete Z_2^e data type
type Z2E e i = ZqBasic ('PP '(Prime2, e)) i
{-
easyPRF :: expr env (PNoise _ (Cyc t r (Z2E k i))) -> expr env (PNoise h (Cyc t s (Z2E 'O i)))
easyPRF x =
  let roundPT = proxy rescaleZqPow2_ (Proxy::Proxy k)
  in roundPT $: (tunnelCyc $: x)


rescaleZqPow2_ :: ex1 e (PreRescaleZqPow2 ex1 k z2 -> z2)
ex1 ~ RescaleToTree ex2
rescaleToTree rescaleZqPow2_ :: ex2 (PreRescale ex2 k z2 -> z2)
ex2 ~ MapCRTSlots ex3 t m
mapCRTSlots $ rescaleToTree rescaleZqPow2_ :: ex3 (Zq2Cyc t m (PreRescale ex2 k z2) -> z2))
-}

rescaleCycPow2_ :: forall t m i k expr bigex z2 e .
  (bigex ~ RescaleToTree (MapCRTSlots expr t m),
   z2 ~ ZqBasic PP2 i, RescaleZqPow2 bigex k z2)
  => Proxy t -> Proxy m -> Tagged (k :: Pos) (expr (Zq2Cyc t m e) (Zq2Cyc t m (PreRescaleZqPow2 bigex k z2 -> z2)))
rescaleCycPow2_ _ _ = mapCRTSlots <$> rescaleToTree <$> (rescaleZqPow2_ :: Tagged k (bigex e (PreRescaleZqPow2 bigex k z2 -> z2)))