{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Crypto.Lol.Cyclotomic.Tensor.Representation
  where

import Crypto.Lol.FactoredDefs


-- | Representation of values of type @r@ in Tensor @t@. For plain backends
-- (Repa, C) this is the identity, but for embedded backends such as Accelerate
-- this will be an embedded term expression.
--
type family TRep (t :: Factored -> * -> *) r :: *

