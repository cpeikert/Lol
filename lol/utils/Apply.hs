{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, PolyKinds, 
             TypeFamilies, TypeOperators #-}

-- applies functions to proxy arguments
module Apply where

class (params :: [k]) `Satisfy` (ctx :: *)  where
  data ArgsCtx ctx

  run :: proxy params
            -> (ArgsCtx ctx -> rnd res) 
            -> [rnd res]

instance '[] `Satisfy` ctx  where
  -- any implementation of ArgsCtx would conflict with concrete instances,
  -- so skip  
  run _ _ = []