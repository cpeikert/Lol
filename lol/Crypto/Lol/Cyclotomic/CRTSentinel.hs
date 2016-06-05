{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Safely exposes a "sentinel" indicating usage of either CRT basis
-- over a base ring, or over its extension ring.  Exactly one of
-- 'CSentinel' or 'ESentinel' has an externally exposed value.

module Crypto.Lol.Cyclotomic.CRTSentinel
( CSentinel, ESentinel
, crtSentinel, crtCSentinel, crtESentinel
, scalarCRTCS, crtCS, crtInvCS, mulGCRTCS, divGCRTCS
, embedCRTCS, twaceCRTCS
) where

import Data.Functor.Trans.Tagged
import Data.Maybe
import Data.Proxy

import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Factored

data CSentinel t m r = CSentinel
data ESentinel t m r = ESentinel

crtSentinel :: (Tensor t, Fact m, CRTrans Maybe r, TElt t r)
               => Either (ESentinel t m r) (CSentinel t m r)
crtSentinel = fromMaybe (Left ESentinel) (Right <$> crtCSentinel)
{-# INLINABLE crtSentinel #-}

crtCSentinel :: forall t m r .
                (Tensor t, Fact m, CRTrans Maybe r, TElt t r)
                => Maybe (CSentinel t m r)
crtCSentinel = proxyT hasCRTFuncs (Proxy::Proxy (t m r)) *>
               pure CSentinel
{-# INLINABLE crtCSentinel #-}

crtESentinel :: (Tensor t, Fact m, CRTrans Maybe r, TElt t r)
                => Maybe (ESentinel t m r)
crtESentinel = case crtSentinel of
  Left  s -> Just s
  Right _ -> Nothing
{-# INLINABLE crtESentinel #-}

scalarCRTCS :: (Tensor t, Fact m, CRTrans Maybe r, TElt t r)
              => CSentinel t m r -> r -> t m r
scalarCRTCS _ = fromJust scalarCRT
{-# INLINABLE scalarCRTCS #-}

crtCS, crtInvCS, mulGCRTCS, divGCRTCS ::
  (Tensor t, Fact m, CRTrans Maybe r, TElt t r)
  => CSentinel t m r -> t m r -> t m r

crtCS     _ = fromJust crt
crtInvCS  _ = fromJust crtInv
mulGCRTCS _ = fromJust mulGCRT
divGCRTCS _ = fromJust divGCRT

{-# INLINABLE crtCS #-}
{-# INLINABLE crtInvCS #-}
{-# INLINABLE mulGCRTCS #-}
{-# INLINABLE divGCRTCS #-}

embedCRTCS :: (Tensor t, m `Divides` m', CRTrans Maybe r, TElt t r)
              => CSentinel t m r -> CSentinel t m' r -> t m r -> t m' r
embedCRTCS _ _ = fromJust embedCRT
{-# INLINABLE embedCRTCS #-}

twaceCRTCS :: (Tensor t, m `Divides` m', CRTrans Maybe r, TElt t r)
              => CSentinel t m' r -> CSentinel t m r -> t m' r -> t m r
twaceCRTCS _ _ = fromJust twaceCRT
{-# INLINABLE twaceCRTCS #-}

