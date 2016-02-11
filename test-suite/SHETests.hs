{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude, RebindableSyntax, 
             DataKinds, TypeOperators, NoMonomorphismRestriction, NoMonoLocalBinds,
             ConstraintKinds, TypeFamilies, FlexibleContexts, PartialTypeSignatures, 
             RankNTypes, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, 
             RebindableSyntax, GADTs, PolyKinds, KindSignatures #-}

module SHETests (sheTests) where

import TestTypes hiding (Zq)

import Control.Applicative
import Control.Monad
import Control.Monad.Random hiding (Rand)
import Control.Monad.State

import Crypto.Lol.LatticePrelude hiding (lift)
import Crypto.Lol.Cyclotomic.Cyc
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.CRTrans
import Crypto.Lol.Gadget
import Crypto.Lol.Cyclotomic.Linear

import Crypto.Lol.Cyclotomic.Tensor.RepaTensor
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as CT
import Crypto.Lol.Cyclotomic.Tensor.CTensor hiding (CT)
import Crypto.Lol.Types.ZqBasic

import Data.Array.Repa.Eval (Elt)
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Storable (Storable)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (generate,output)
import Test.QuickCheck.Monadic (monadicIO, assert)

import Utils
import Harness.SHE

import Data.Promotion.Prelude.List

v = 1 :: Double

sheTests = 
  [testGroupRnd "Dec . Enc"  $ benchDec (Proxy::Proxy DecParams) $ wrap' prop_encDec,
   testGroupRnd "DecU . Enc" $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' prop_encDecU,
   testGroupRnd "AddPub"    $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' prop_addPub,
   testGroupRnd "MulPub"    $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' prop_mulPub,
   testGroupRnd "ScalarPub" $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' prop_addScalar,
   testGroupRnd "CTAdd"     $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' prop_ctadd,
   testGroupRnd "CTMul"     $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' prop_ctmul,
   testGroupRnd "CT zero"   $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' prop_ctzero,
   testGroupRnd "CT one"    $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' prop_ctone,
   testGroupRnd "ModSwitch PT" modSwPTTests,
   testGroupRnd "Tunnel" tunnelTests,
   testGroupRnd "Twace" $ benchCTTwEm (Proxy::Proxy TwoIdxParams) $ wrap' prop_cttwace,
   testGroupRnd "Embed" $ benchCTTwEm (Proxy::Proxy TwoIdxParams) $ wrap' prop_ctembed,
   testGroupRnd "KSLin" $ benchKSQ (Proxy::Proxy KSQParams) $ wrap' prop_ksLin,
   testGroupRnd "keySwitch" $ benchKSQ (Proxy::Proxy KSQParams) $ wrap' prop_ksQuad
  ]

type CTCombos = '[
  '(F7, F7, ZP2,ZQ2),
  '(F7, F21,ZP2,ZQ2),
  '(F2, F8, ZP2,Zq 536871001),
  '(F1, F8, ZP2,Zq 536871001),
  '(F4, F12,ZP2,SmoothZQ1),
  '(F4, F8, ZP3,Zq SmoothQ1),
  '(F7, F7, ZP4,ZQ2),
  '(F7, F21,ZP4,ZQ2),
  '(F1, F4, ZP4,ZQ1),
  '(F4, F4, ZP4,ZQ1),
  '(F14,F14,ZP4,ZQ1),
  '(F28,F28,ZP4,ZQ1),
  '(F28,F28,ZP4,Zq 80221),
  '(F1, F8, ZP4,Zq 536871001),
  '(F2, F8, ZP4,Zq 536871001),
  '(F4, F12,ZP8,SmoothZQ1)
  ]

type Gadgets = '[TrivGad, BaseBGad 2]
type Tensors = '[CT.CT,RT]
type MM'PQCombos = 
  '[ '(F1, F7, ZP2, ZQ2),
     '(F2, F4, ZP8, SmoothZQ2),
     '(F4, F12, ZP2, SmoothZQ2),
     '(F8, F64, ZP2, SmoothZQ2),
     '(F3, F27, ZP2, SmoothZQ2),
     '(F2, F4, ZP8, SmoothZQ3),
     '(F4, F12, ZP2, SmoothZQ3),
     '(F8, F64, ZP2, SmoothZQ3),
     '(F3, F27, ZP2, SmoothZQ3)]


type CTParams  = ( '(,) <$> Tensors) <*> CTCombos
type DecParams = ( '(,) <$> Tensors) <*> (Nub (Filter Liftable CTCombos))
type Zq'Params = ( '(,) <$> Tensors) <*> (Map AddZq (Filter NonLiftable MM'PQCombos))
type KSQParams = ( '(,) <$> Gadgets) <*> Zq'Params
type TwoIdxParams = ( '(,) <$> Tensors) <*> '[ '(F1, F7, F3, F21, Zq 2, ZQ1)]

prop_ksLin :: (DecryptUCtx t m m' z zp zq, Eq (Cyc t m zp))
  => SK (Cyc t m' z) 
     -> KSLinear t m m' z zp zq zq' gad 
     -> CT m zp (Cyc t m' zq) 
     -> TestBool '(t,m,m',zp,zq,zq',gad)
prop_ksLin skin (KSL kswlin skout) x =
  let x' = decryptUnrestricted skin x
      y = kswlin x
      y' = decryptUnrestricted skout y
  in test $ x' == y'

prop_ksQuad :: (Ring (CT m zp (Cyc t m' zq)),
                DecryptUCtx t m m' z zp zq, 
                Eq (Cyc t m zp))
  => SK (Cyc t m' z) 
     -> KSHint m zp t m' zq gad zq' 
     -> CT m zp (Cyc t m' zq) 
     -> CT m zp (Cyc t m' zq) 
     -> TestBool '(t,m,m',zp,zq,zq',gad)
prop_ksQuad sk (KeySwitch kswq) x1 x2 = 
  let x' = kswq $ x1*x2
      y1 = decryptUnrestricted sk x1
      y2 = decryptUnrestricted sk x2
      y = y1*y2
      x = decryptUnrestricted sk x'
  in test $ y == x

prop_addPub :: forall t m m' z zp zq . 
  (DecryptUCtx t m m' z zp zq,
   AddPublicCtx t m m' zp zq,
   Eq (Cyc t m zp))
  => SK (Cyc t m' z) 
     -> Cyc t m zp 
     -> CT m zp (Cyc t m' zq) 
     -> TestBool '(t,m,m',zp,zq)
prop_addPub sk x y = 
  let xy = addPublic x y
      xy' = decryptUnrestricted sk xy
      y' = decryptUnrestricted sk y
  in test $ xy' == (x+y')

prop_mulPub :: (DecryptUCtx t m m' z zp zq,
                MulPublicCtx t m m' zp zq,
                Eq (Cyc t m zp))
  => SK (Cyc t m' z) 
     -> Cyc t m zp 
     -> CT m zp (Cyc t m' zq) 
     -> TestBool '(t,m,m',zp,zq)
prop_mulPub sk x y = 
  let xy = mulPublic x y
      xy' = decryptUnrestricted sk xy
      y' = decryptUnrestricted sk y
  in test $ xy' == (x*y')

prop_addScalar :: (DecryptUCtx t m m' z zp zq,
                   AddScalarCtx t m' zp zq,
                   Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> Rand zp -> CT m zp (Cyc t m' zq) -> TestBool '(t,m,m',zp,zq)
prop_addScalar sk (Rand c) x =
  let cx = addScalar c x
      cx' = decryptUnrestricted sk cx
      x' = decryptUnrestricted sk x
  in test $ cx' == ((scalarCyc c)+x')

prop_ctadd :: (DecryptUCtx t m m' z zp zq,
               Additive (CT m zp (Cyc t m' zq)),
               Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq) -> TestBool '(t,m,m',zp,zq)
prop_ctadd sk x1 x2 = 
  let x1' = decryptUnrestricted sk x1
      x2' = decryptUnrestricted sk x2
      y = x1+x2
      y' = decryptUnrestricted sk y
  in test $ x1'+x2' == y'

prop_ctmul :: (DecryptUCtx t m m' z zp zq,
               Ring (CT m zp (Cyc t m' zq)),
               Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq) -> TestBool '(t,m,m',zp,zq)
prop_ctmul sk x1 x2 = 
  let x1' = decryptUnrestricted sk x1
      x2' = decryptUnrestricted sk x2
      y = x1*x2
      y' = decryptUnrestricted sk y
  in test $ x1'*x2' == y'

prop_ctzero :: forall t m m' z zp zq .
 (DecryptUCtx t m m' z zp zq,
  Additive (CT m zp (Cyc t m' zq)),
  Eq (Cyc t m zp)) 
  => SK (Cyc t m' z) -> TestBool '(t,m,m',zp,zq)
prop_ctzero sk =
  let z = decryptUnrestricted sk (zero :: CT m zp (Cyc t m' zq))
  in test $ zero == z

prop_ctone :: forall t m m' z zp zq .
  (DecryptUCtx t m m' z zp zq,
   Ring (CT m zp (Cyc t m' zq)),
   Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> TestBool '(t,m,m',zp,zq)
prop_ctone sk = 
  let z = decryptUnrestricted sk (one :: CT m zp (Cyc t m' zq))
  in test $ one == z

prop_ctembed :: forall t r r' s s' z zp zq . 
  (DecryptUCtx t r r' z zp zq,
   DecryptUCtx t s s' z zp zq,
   r `Divides` r', 
   s `Divides` s', 
   r `Divides` s, 
   r' `Divides` s',
   Eq (Cyc t s zp))
  => SK (Cyc t r' z) -> CT r zp (Cyc t r' zq) -> TestBool '(t,r,r',s,s',zp,zq)
prop_ctembed sk x = 
  let y = embedCT x :: CT s zp (Cyc t s' zq)
      y' = decryptUnrestricted (embedSK sk) y
      x' = decryptUnrestricted sk x
  in test $ (embed x' :: Cyc t s zp) == y'

-- CT must be encrypted with key from small ring
prop_cttwace :: forall t r r' s s' z zp zq . 
  (EncryptCtx t s s' z zp zq, 
   DecryptUCtx t r r' z zp zq,
   r `Divides` s,
   r' `Divides` s',
   s `Divides` s',
   r ~ (FGCD r' s))
  => SK (Cyc t r' z) -> Cyc t s zp -> TestBoolRnd '(t,r,r',s,s',zp,zq)
prop_cttwace sk x = TestBoolRnd $ do
  y :: CT s zp (Cyc t s' zq) <- encrypt (embedSK sk) x
  let y' = twaceCT y :: CT r zp (Cyc t r' zq)
      x' = decryptUnrestricted sk y'
  return $ (twace x :: Cyc t r zp) == x'

prop_encDecU :: forall t m m' z zp zq . 
  (GenSKCtx t m' z Double, 
   EncryptCtx t m m' z zp zq, 
   DecryptUCtx t m m' z zp zq,
   Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> Cyc t m zp -> TestBoolRnd '(t,m,m',zp,zq)
prop_encDecU sk x = TestBoolRnd $ do
  y :: CT m zp (Cyc t m' zq) <- encrypt sk x
  let x' = decryptUnrestricted sk $ y
  return $ x == x'

prop_encDec :: forall t m m' z zp zq . 
  (GenSKCtx t m' z Double, 
   EncryptCtx t m m' z zp zq, 
   DecryptCtx t m m' z zp zq,
   Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> Cyc t m zp -> TestBoolRnd '(t,m,m',zp,zq)
prop_encDec sk x = TestBoolRnd $ do
  y :: CT m zp (Cyc t m' zq) <- encrypt sk x
  let x' = decrypt sk $ y
  return $ x == x'

-- one-off tests, no wrapper
prop_modSwPT :: forall t m m' z zp zp' zq .
  (DecryptUCtx t m m' z zp zq,
   DecryptUCtx t m m' z zp' zq,
   ModSwitchPTCtx t m' zp zp' zq,
   RescaleCyc (Cyc t) zp zp',
   Ring (Cyc t m zp),
   Eq (Cyc t m zp'),
   Mod zp, Mod zp',
   ModRep zp ~ ModRep zp') 
  => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> TestBool '(t, '(m,m',zp,zp',zq))
prop_modSwPT sk y =
  let p = proxy modulus (Proxy::Proxy zp)
      p' = proxy modulus (Proxy::Proxy zp')
      x = decryptUnrestricted sk y
      x' = (fromIntegral $ p `div` p') * x
      y' = modSwitchPT y :: CT m zp' (Cyc t m' zq)
      x'' = decryptUnrestricted sk y'
  in test $ x'' == rescaleCyc Dec x'

modSwPTTests = (modSwPTTests' (Proxy::Proxy CT.CT)) ++ (modSwPTTests' (Proxy::Proxy RT))

helper :: (Proxy '(t,b) -> a) -> Proxy t -> Proxy b -> a
helper f _ _ = f Proxy

modSwPTTests' p = 
  [helper (wrap' prop_modSwPT) p (Proxy::Proxy '(F7,F21,Zq 4,Zq 8, Zq 18869761)),
   helper (wrap' prop_modSwPT) p (Proxy::Proxy '(F7,F42,Zq 2,Zq 4, Zq (18869761 ** 19393921)))]

tunnelTests = (tunnelTests' (Proxy::Proxy CT.CT)) ++ (tunnelTests' (Proxy::Proxy RT))

tunnelTests' p = 
  [helper (wrap' prop_ringTunnel) p 
    (Proxy::Proxy '(F8,F40,F20,F60,Zq 4,Zq (18869761 ** 19393921),TrivGad))]

prop_ringTunnel :: forall t e r s e' r' s' z zp zq gad . 
  (TunnelCtx t e r s e' r' s' z zp zq gad,
   EncryptCtx t r r' z zp zq,
   GenSKCtx t r' z Double,
   GenSKCtx t s' z Double,
   DecryptUCtx t s s' z zp zq,
   Random zp,
   e ~ FGCD r s, Fact e) 
  => Cyc t r zp -> TestBoolRnd '(t,'(r,r',s,s',zp,zq,gad))
prop_ringTunnel x = TestBoolRnd $ do
  let totr = proxy totientFact (Proxy::Proxy r)
      tote = proxy totientFact (Proxy::Proxy e)
      basisSize = totr `div` tote
  -- choose a random linear function of the appropriate size
  bs :: [Cyc t s zp] <- replicateM basisSize getRandom
  let f = (linearDec bs) \\ (gcdDivides (Proxy::Proxy r) (Proxy::Proxy s)) :: Linear t zp e r s 
      expected = evalLin f x \\ (gcdDivides (Proxy::Proxy r) (Proxy::Proxy s))
  skin :: SK (Cyc t r' (LiftOf zp)) <- genSK v
  skout :: SK (Cyc t s' (LiftOf zp)) <- genSK v
  y :: CT r zp (Cyc t r' zq) <- encrypt skin x
  tunn <- proxyT (tunnelCT f skout skin) (Proxy::Proxy gad)
  let y' = tunn y :: CT s zp (Cyc t s' zq)
      actual = decryptUnrestricted skout y' :: Cyc t s zp
  return $ expected == actual

