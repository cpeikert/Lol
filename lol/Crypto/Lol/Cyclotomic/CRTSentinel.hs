{-# LANGUAGE FlexibleContexts, PolyKinds, ScopedTypeVariables #-}

module Crypto.Lol.Cyclotomic.CRTSentinel
( CSentinel, ESentinel, crtSentinel, crtCSentinel, crtESentinel
) where

import Data.Maybe
import Data.Proxy
import Data.Functor.Trans.Tagged

import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.CRTrans
import Crypto.Lol.Factored

data CSentinel t m r = CSentinel
data ESentinel t m r = ESentinel

crtSentinel :: (Tensor t, Fact m, CRTrans Maybe r, TElt t r)
               => Either (ESentinel t m r) (CSentinel t m r)
crtSentinel = fromMaybe (Left ESentinel) (Right <$> crtCSentinel)

crtCSentinel :: forall t m r .
                (Tensor t, Fact m, CRTrans Maybe r, TElt t r)
                => Maybe (CSentinel t m r)
crtCSentinel = proxyT hasCRTFuncs (Proxy::Proxy (t m r)) *>
               pure CSentinel

crtESentinel :: (Tensor t, Fact m, CRTrans Maybe r, TElt t r)
                => Maybe (ESentinel t m r)
crtESentinel = case crtSentinel of
  Left  s -> Just s
  Right _ -> Nothing
