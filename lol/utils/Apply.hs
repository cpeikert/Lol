{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, PolyKinds,
             TypeFamilies, TypeOperators #-}

-- applies functions to proxy arguments
module Apply where

-- not associated due to the generic instance below:
-- any definition of ArgsCtx would conflict with specific instances
data family ArgsCtx ctx

class (params :: [k]) `Satisfy` (ctx :: *)  where
  run :: proxy params
            -> (ArgsCtx ctx -> rnd res)
            -> [rnd res]

instance '[] `Satisfy` ctx  where
  run _ _ = []