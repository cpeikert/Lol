{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, PolyKinds, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies, TypeOperators #-}

module SHETests (sheTests) where

import Harness.SHE
import Tests
import Utils

import Control.Monad
import Control.Monad.Random

import Crypto.Lol hiding (CT)
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Cyclotomic.Linear
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as CT

v = 1 :: Double

sheTests = 
  [testGroup "Dec . Enc"  $ applyDec (Proxy::Proxy DecParams) $ hideSHEArgs prop_encDec,
   testGroup "DecU . Enc" $ applyCTFunc (Proxy::Proxy CTParams) $ hideSHEArgs prop_encDecU,
   testGroup "AddPub"     $ applyCTFunc (Proxy::Proxy CTParams) $ hideSHEArgs prop_addPub,
   testGroup "MulPub"     $ applyCTFunc (Proxy::Proxy CTParams) $ hideSHEArgs prop_mulPub,
   testGroup "ScalarPub"  $ applyCTFunc (Proxy::Proxy CTParams) $ hideSHEArgs prop_addScalar,
   testGroup "CTAdd"      $ applyCTFunc (Proxy::Proxy CTParams) $ hideSHEArgs prop_ctadd,
   testGroup "CTMul"      $ applyCTFunc (Proxy::Proxy CTParams) $ hideSHEArgs prop_ctmul,
   testGroup "CT zero"    $ applyCTFunc (Proxy::Proxy CTParams) $ hideSHEArgs prop_ctzero,
   testGroup "CT one"     $ applyCTFunc (Proxy::Proxy CTParams) $ hideSHEArgs prop_ctone,
   testGroup "ModSwitch PT" modSwPTTests,
   testGroup "Tunnel" tunnelTests,
   testGroup "Twace"      $ applyCTTwEm (Proxy::Proxy TwoIdxParams) $ hideSHEArgs prop_cttwace,
   testGroup "Embed"      $ applyCTTwEm (Proxy::Proxy TwoIdxParams) $ hideSHEArgs prop_ctembed,
   testGroup "KSLin"      $ applyKSQ (Proxy::Proxy KSQParams) $ hideSHEArgs prop_ksLin,
   testGroup "keySwitch"  $ applyKSQ (Proxy::Proxy KSQParams) $ hideSHEArgs prop_ksQuad
  ]

type CTCombos = '[
  '(F7, F7, Zq 2,Zq (19393921 ** 18869761)),
  '(F7, F21,Zq 2,Zq (19393921 ** 18869761)),
  '(F2, F8, Zq 2,Zq 536871001),
  '(F1, F8, Zq 2,Zq 536871001),
  '(F4, F12,Zq 2,Zq 2148249601),
  '(F4, F8, Zq 3,Zq 2148249601),
  '(F7, F7, Zq 4,Zq (19393921 ** 18869761)),
  '(F7, F21,Zq 4,Zq (19393921 ** 18869761)),
  '(F1, F4, Zq 4,Zq 18869761),
  '(F4, F4, Zq 4,Zq 18869761),
  '(F14,F14,Zq 4,Zq 18869761),
  '(F28,F28,Zq 4,Zq 18869761),
  '(F28,F28,Zq 4,Zq 80221),
  '(F1, F8, Zq 4,Zq 536871001),
  '(F2, F8, Zq 4,Zq 536871001),
  '(F4, F12,Zq 8,Zq 2148249601)
  ]

type Gadgets = '[TrivGad, BaseBGad 2]
type Tensors = '[CT.CT,RT]
type MM'PQCombos = 
  '[ '(F1, F7, Zq 2, Zq (19393921 ** 18869761)),
     '(F2, F4, Zq 8, Zq (2148854401 ** 2148249601)),
     '(F4, F12, Zq 2, Zq (2148854401 ** 2148249601)),
     '(F8, F64, Zq 2, Zq (2148854401 ** 2148249601)),
     '(F3, F27, Zq 2, Zq (2148854401 ** 2148249601)),
     '(F2, F4, Zq 8, Zq (2148854401 ** 2148249601 ** 2150668801)),
     '(F4, F12, Zq 2, Zq (2148854401 ** 2148249601 ** 2150668801)),
     '(F8, F64, Zq 2, Zq (2148854401 ** 2148249601 ** 2150668801)),
     '(F3, F27, Zq 2, Zq (2148854401 ** 2148249601 ** 2150668801))]


type CTParams  = ( '(,) <$> Tensors) <*> CTCombos
type DecParams = ( '(,) <$> Tensors) <*> (Nub (Filter Liftable CTCombos))
type Zq'Params = ( '(,) <$> Tensors) <*> (Map AddZq (Filter NonLiftable MM'PQCombos))
type KSQParams = ( '(,) <$> Gadgets) <*> Zq'Params
type TwoIdxParams = ( '(,) <$> Tensors) <*> '[ '(F1, F7, F3, F21, Zq 2, Zq 18869761)]

prop_ksLin :: (DecryptUCtx t m m' z zp zq, Eq (Cyc t m zp))
  => SK (Cyc t m' z) 
     -> KSLinear t m m' z zp zq zq' gad 
     -> CT m zp (Cyc t m' zq) 
     -> Test '(t,m,m',zp,zq,zq',gad)
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
     -> Test '(t,m,m',zp,zq,zq',gad)
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
     -> Test '(t,m,m',zp,zq)
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
     -> Test '(t,m,m',zp,zq)
prop_mulPub sk x y = 
  let xy = mulPublic x y
      xy' = decryptUnrestricted sk xy
      y' = decryptUnrestricted sk y
  in test $ xy' == (x*y')

prop_addScalar :: (DecryptUCtx t m m' z zp zq,
                   AddScalarCtx t m' zp zq,
                   Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> Arg zp -> CT m zp (Cyc t m' zq) -> Test '(t,m,m',zp,zq)
prop_addScalar sk (Arg c) x =
  let cx = addScalar c x
      cx' = decryptUnrestricted sk cx
      x' = decryptUnrestricted sk x
  in test $ cx' == ((scalarCyc c)+x')

prop_ctadd :: (DecryptUCtx t m m' z zp zq,
               Additive (CT m zp (Cyc t m' zq)),
               Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq) -> Test '(t,m,m',zp,zq)
prop_ctadd sk x1 x2 = 
  let x1' = decryptUnrestricted sk x1
      x2' = decryptUnrestricted sk x2
      y = x1+x2
      y' = decryptUnrestricted sk y
  in test $ x1'+x2' == y'

prop_ctmul :: (DecryptUCtx t m m' z zp zq,
               Ring (CT m zp (Cyc t m' zq)),
               Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq) -> Test '(t,m,m',zp,zq)
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
  => SK (Cyc t m' z) -> Test '(t,m,m',zp,zq)
prop_ctzero sk =
  let z = decryptUnrestricted sk (zero :: CT m zp (Cyc t m' zq))
  in test $ zero == z

prop_ctone :: forall t m m' z zp zq .
  (DecryptUCtx t m m' z zp zq,
   Ring (CT m zp (Cyc t m' zq)),
   Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> Test '(t,m,m',zp,zq)
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
  => SK (Cyc t r' z) -> CT r zp (Cyc t r' zq) -> Test '(t,r,r',s,s',zp,zq)
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
  => SK (Cyc t r' z) -> Cyc t s zp -> Test '(t,r,r',s,s',zp,zq)
prop_cttwace sk x = testIO $ do
  y :: CT s zp (Cyc t s' zq) <- encrypt (embedSK sk) x
  let y' = twaceCT y :: CT r zp (Cyc t r' zq)
      x' = decryptUnrestricted sk y'
  return $ (twace x :: Cyc t r zp) == x'

prop_encDecU :: forall t m m' z zp zq . 
  (GenSKCtx t m' z Double, 
   EncryptCtx t m m' z zp zq, 
   DecryptUCtx t m m' z zp zq,
   Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> Cyc t m zp -> Test '(t,m,m',zp,zq)
prop_encDecU sk x = testIO $ do
  y :: CT m zp (Cyc t m' zq) <- encrypt sk x
  let x' = decryptUnrestricted sk $ y
  return $ x == x'

prop_encDec :: forall t m m' z zp zq . 
  (GenSKCtx t m' z Double, 
   EncryptCtx t m m' z zp zq, 
   DecryptCtx t m m' z zp zq,
   Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> Cyc t m zp -> Test '(t,m,m',zp,zq)
prop_encDec sk x = testIO $ do
  y :: CT m zp (Cyc t m' zq) <- encrypt sk x
  let x' = decrypt sk $ y
  return $ x == x'

helper :: (Proxy '(t,b) -> a) -> Proxy t -> Proxy b -> a
helper f _ _ = f Proxy

-- one-off tests, no hideSHEArgsper
prop_modSwPT :: forall t m m' z zp zp' zq .
  (DecryptUCtx t m m' z zp zq,
   DecryptUCtx t m m' z zp' zq,
   ModSwitchPTCtx t m' zp zp' zq,
   RescaleCyc (Cyc t) zp zp',
   Ring (Cyc t m zp),
   Eq (Cyc t m zp'),
   Mod zp, Mod zp',
   ModRep zp ~ ModRep zp') 
  => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> Test '(t, '(m,m',zp',zp,zq))
prop_modSwPT sk y =
  let p = proxy modulus (Proxy::Proxy zp)
      p' = proxy modulus (Proxy::Proxy zp')
      z = (fromIntegral $ p `div` p')*y
      x = decryptUnrestricted sk z
      y' = modSwitchPT z :: CT m zp' (Cyc t m' zq)
      x'' = decryptUnrestricted sk y'
  in test $ x'' == rescaleCyc Dec x

modSwPTTests = (modSwPTTests' (Proxy::Proxy CT.CT)) ++ (modSwPTTests' (Proxy::Proxy RT))

modSwPTTests' p = 
  [helper (hideSHEArgs prop_modSwPT) p (Proxy::Proxy '(F7,F21,Zq 4,Zq 8,Zq 18869761)),
   helper (hideSHEArgs prop_modSwPT) p (Proxy::Proxy '(F7,F42,Zq 2,Zq 4,Zq (18869761 ** 19393921)))]


tunnelTests = (tunnelTests' (Proxy::Proxy CT.CT)) ++ (tunnelTests' (Proxy::Proxy RT))

tunnelTests' p = 
  [helper (hideSHEArgs prop_ringTunnel) p 
    (Proxy::Proxy '(F8,F40,F20,F60,Zq 4,Zq (18869761 ** 19393921),TrivGad))]

prop_ringTunnel :: forall t e r s e' r' s' z zp zq gad . 
  (TunnelCtx t e r s e' r' s' z zp zq gad,
   EncryptCtx t r r' z zp zq,
   GenSKCtx t r' z Double,
   GenSKCtx t s' z Double,
   DecryptUCtx t s s' z zp zq,
   Random zp,
   e ~ FGCD r s, Fact e) 
  => Cyc t r zp -> Test '(t,'(r,r',s,s',zp,zq,gad))
prop_ringTunnel x = testIO $ do
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

