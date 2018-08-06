{-|
Module      : Crypto.Lol.Cyclotomic.Linear
Description : Functions from one cyclotomic ring to another that are linear
              over a common subring.
Copyright   : (c) Eric Crockett, 2011-2018
                  Chris Peikert, 2011-2018
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

  \( \def\lcm{\text{lcm}} \)

Functions from one cyclotomic ring to another that are linear
over a common subring.
-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Lol.Cyclotomic.Linear
( Linear, ExtendLinCtx
, linearDec, evalLin, liftLin, extendLin
) where

import Crypto.Lol.Cyclotomic.Language
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Prelude
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto
import Crypto.Proto.Lol.LinearRq      (LinearRq (LinearRq))
import Crypto.Proto.Lol.RqProduct     (RqProduct)

import Algebra.Additive as Additive (C)

import Control.Applicative
import Control.DeepSeq

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import           Data.Word

-- | An \(E\)-linear function from \(R\) to \(S\).

-- CJP: also have constructor for relative Pow basis of R/E?  So far
-- not needed.
newtype Linear c (e::Factored) (r::Factored) (s::Factored) z = RD [c s z]
  deriving Show

deriving instance NFData (c s z) => NFData (Linear c e r s z)

-- some params are phantom but matter for safety
type role Linear representational representational representational nominal nominal

-- | Construct an \(E\)-linear function given a list of its output values
-- (in \(S\)) on the relative decoding basis of \(R/E\).  The number of
-- elements in the list must not exceed the size of the basis.
linearDec :: forall c e r s z .
             (e `Divides` r, e `Divides` s, Cyclotomic (c s z), ExtensionCyc c z)
             => [c s z] -> Linear c e r s z
linearDec ys = let ps = proxy powBasis (Proxy::Proxy e) `asTypeOf` ys
               in if length ys <= length ps then RD (adviseCRT <$> ys)
               else error $ "linearDec: too many entries: "
                        ++ show (length ys) ++ " versus "
                        ++ show (length ps)

-- | Evaluates the given linear function on the input.
evalLin :: forall c e r s z .
           (e `Divides` r, e `Divides` s, Ring (c s z), ExtensionCyc c z)
           => Linear c e r s z -> c r z -> c s z
evalLin (RD ys) r = sum (zipWith (*) ys $ embed <$> (coeffsDec r :: [c e z]))

instance Additive (c s z) => Additive.C (Linear c e r s z) where
  zero = RD []

  (RD as) + (RD bs) = RD $ sumall as bs
    where sumall [] ys = ys
          sumall xs [] = xs
          sumall (x:xs) (y:ys) = x+y : sumall xs ys

  negate (RD as) = RD $ negate <$> as

instance (Reduce (c s z) (c s zp))
  => Reduce (Linear c e r s z) (Linear c e r s zp) where
  reduce (RD ys) = RD $ reduce <$> ys

type instance LiftOf (Linear c e r s zp) = Linear c e r s (LiftOf zp)

-- | Lift the linear function in the specified basis (or any, if
-- 'Nothing' is given).  The powerful basis is generally best,
-- geometrically.
liftLin :: (LiftCyc (c s) zp)
  => Maybe Basis -> Linear c e r s zp -> Linear c e r s (LiftOf zp)
liftLin b (RD ys) = RD $ liftCyc b <$> ys

-- | A convenient constraint synonym for extending a linear function
-- to larger rings.
type ExtendLinCtx c e r s e' r' s' z =
  (e ~ FGCD r e',                   -- E = R \cap E'
   FLCM r e' `Divides` r',          -- (R+E') \subseteq R'
   e' `Divides` s', s `Divides` s', -- (S+E') \subseteq S'
   ExtensionCyc c z, Additive (c s' z))

-- | Extend an \(E\)-linear function \(R\to S\) to an \(E'\)-linear
-- function \(R'\to S'\).
extendLin :: forall c e r s e' r' s' z .
  (ExtendLinCtx c e r s e' r' s' z)
  => Linear c e r s z -> Linear c e' r' s' z
-- need the indexing:
-- dec_{R'/E'} <-> dec_{R'/(R+E')} \otimes dec_{(R+E')/E'}
--               = dec_{R'/(R+E')} \otimes dec_{R/E}.
extendLin (RD ys) =
  let yvec  = V.fromList ys
      idxs  = proxy baseIndicesPow (Proxy :: Proxy '(FLCM r e', r'))
      y'vec = V.generate (U.length idxs) $ \idx ->
        let (j0,j1) = idxs U.! idx
        in if j0 == 0 then embed (yvec V.! j1) else zero
  in RD $ V.toList y'vec

instance (Reflects e Word32, Reflects r Word32,
          Protoable (c s zq), ProtoType (c s zq) ~ RqProduct)
  => Protoable (Linear c e r s zq) where

  type ProtoType (Linear c e r s zq) = LinearRq

  toProto (RD cs) =
    LinearRq (proxy value (Proxy::Proxy e)) (proxy value (Proxy::Proxy r)) $ toProto cs

  fromProto (LinearRq e r cs) =
    let e' = proxy value (Proxy::Proxy e)
        r' = proxy value (Proxy::Proxy r)
    in if e == e' && r == r'
       then RD <$> fromProto cs
       else error $ "Could not deserialize Linear: types imply e=" ++
              show e' ++ " and r=" ++ show r' ++
              ", but serializd object is for e=" ++
              show e ++ " and r=" ++ show r
