{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module ZqTests (zqTests) where

import Tests
import Utils
import Gen
import Apply

import Crypto.Lol
import Crypto.Lol.CRTrans

import Control.Monad.Random
import qualified Test.Framework as TF

-- #if ACCELERATE_TENSOR_ENABLE
-- import Crypto.Lol.Cyclotomic.Tensor.Accelerate
-- #endif

-- TLM: These tests need to lift the value 'r' into 'TRep t r', and then we need
-- to be able to eval 'TRep t r -> r' (or similar).
--


data BasicCtxD
type BasicCtx t r = (Field r, Tensor t, TElt t r, Eq r, Eq (TRep t r), Random r, ShowType '(t,r), CRTEmbed (TRep t r))

instance (params `Satisfy` BasicCtxD, BasicCtx t r) => ( '(t,r) ': params) `Satisfy` BasicCtxD where
  data ArgsCtx BasicCtxD where
    BC :: BasicCtx t r => Proxy '(t,r) -> ArgsCtx BasicCtxD
  --
  run _ f = (f $ BC (Proxy::Proxy '(t,r))) : (run (Proxy::Proxy params) f)

applyBasic
    :: (params `Satisfy` BasicCtxD, MonadRandom rnd)
    => Proxy params
    -> (forall t r . (BasicCtx t r, Generatable rnd r) => Proxy '(t,r) -> rnd res)
    -> [rnd res]
applyBasic params g = run params $ \(BC p) -> g p


prop_add :: forall t r. (Ring r, Eq r) => Integer -> Integer -> Test '(t,r)
prop_add x y = test $ (fromIntegral $ x + y) == ((fromIntegral x) + (fromIntegral y :: r))

prop_mul :: forall t r . (Ring r, Eq r) => Integer -> Integer -> Test '(t,r)
prop_mul x y = test $ (fromIntegral $ x * y) == ((fromIntegral x) * (fromIntegral y :: r))

prop_recip :: forall t r . (Field r, Eq r) => r -> Test '(t,r)
prop_recip x = test $ (x == 0) || (one == (x * recip x))

-- tests that multiplication in the extension ring matches CRT multiplication
prop_mul_ext
    :: forall t r . (Tensor t, TElt t r, CRTEmbed (TRep t r), Ring (TRep t r), Eq (TRep t r))
    => Proxy '(t,r)
    -> r
    -> r
    -> Test '(t,r)
prop_mul_ext _ x y = test $
  let x' = proxy (constant x) (Proxy :: Proxy t)
      y' = proxy (constant y) (Proxy :: Proxy t)
      --
      z  = x' * y'
      z' = fromExt (toExt x' * toExt y')
  in
  z == z'


type Tensors =
  '[CT,RT
-- #if ACCELERATE_TENSOR_ENABLE
--    ,AT
-- #endif
   ]

type ZqTypes = '[
    Zq 3
  , Zq (3 ** 5)
  , Zq (3 ** 5 ** 7)
  ]

type ZqParams = '(,) <$> Tensors <*> ZqTypes

zqTests :: [TF.Test]
zqTests =
  [ testGroupM "(+)"                $ applyBasic (Proxy::Proxy ZqParams) $ hideArgs prop_add
  , testGroupM "(*)"                $ applyBasic (Proxy::Proxy ZqParams) $ hideArgs prop_mul
  , testGroupM "^-1"                $ applyBasic (Proxy::Proxy ZqParams) $ hideArgs prop_recip
  , testGroupM "extension ring (*)" $ applyBasic (Proxy::Proxy ZqParams) $ hideArgs prop_mul_ext
  ]

