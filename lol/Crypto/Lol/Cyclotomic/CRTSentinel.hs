{-|
Module      : Crypto.Lol.Cyclotomic.CRTSentinel
Description : Safely exposes a "sentinel" indicating usage of either CRT basis
              over a base ring, or over its extension ring.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

Safely exposes a "sentinel" indicating usage of either CRT basis
over a base ring, or over its extension ring.  Exactly one of
'CSentinel' or 'ESentinel' has an externally exposed value.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Crypto.Lol.Cyclotomic.CRTSentinel
( CSentinel, ESentinel
, crtSentinel, crtCSentinel, crtESentinel
, scalarCRTCS, crtCS, crtInvCS, mulGCRTCS, divGCRTCS
, embedCRTCS, twaceCRTCS
) where

import Data.Maybe

import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Factored

data CSentinel t m r = CSentinel deriving (Eq, Show)
data ESentinel t m r = ESentinel deriving (Eq, Show)

crtSentinel :: (TensorCRT t Maybe r, Fact m)
               => Either (ESentinel t m r) (CSentinel t m r)
crtSentinel = fromMaybe (Left ESentinel) (Right <$> crtCSentinel)
{-# INLINABLE crtSentinel #-}

crtCSentinel :: forall t m r .
                (TensorCRT t Maybe r, Fact m)
                => Maybe (CSentinel t m r)
crtCSentinel = hasCRTFuncs @t @m @r *> pure CSentinel
{-# INLINABLE crtCSentinel #-}

crtESentinel :: (TensorCRT t Maybe r, Fact m)
                => Maybe (ESentinel t m r)
crtESentinel = case crtSentinel of
  Left  s -> Just s
  Right _ -> Nothing
{-# INLINABLE crtESentinel #-}

scalarCRTCS :: (TensorCRT t Maybe r, Fact m)
              => CSentinel t m r -> r -> t m r
scalarCRTCS _ = fromJust scalarCRT
{-# INLINABLE scalarCRTCS #-}

crtCS, crtInvCS, mulGCRTCS, divGCRTCS ::
  (TensorCRT t Maybe r, Fact m)
  => CSentinel t m r -> t m r -> t m r

crtCS     _ = fromJust crt
crtInvCS  _ = fromJust crtInv
mulGCRTCS _ = fromJust mulGCRT
divGCRTCS _ = fromJust divGCRT

{-# INLINABLE crtCS #-}
{-# INLINABLE crtInvCS #-}
{-# INLINABLE mulGCRTCS #-}
{-# INLINABLE divGCRTCS #-}

embedCRTCS :: (TensorCRT t Maybe r, m `Divides` m')
              => CSentinel t m r -> CSentinel t m' r -> t m r -> t m' r
embedCRTCS _ _ = fromJust embedCRT
{-# INLINABLE embedCRTCS #-}

twaceCRTCS :: (TensorCRT t Maybe r, m `Divides` m')
              => CSentinel t m' r -> CSentinel t m r -> t m' r -> t m r
twaceCRTCS _ _ = fromJust twaceCRT
{-# INLINE twaceCRTCS #-}

