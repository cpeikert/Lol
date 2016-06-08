{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, PolyKinds, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies, TypeOperators #-}

module SHETests (sheTests) where

import Apply.SHE
import GenArgs
import GenArgs.SHE
import Tests hiding (hideArgs)
import Utils

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Control.Monad.State

import Crypto.Lol hiding (CT)
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Cyclotomic.Linear
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as CT

import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2

v :: Double
v = 1

hideArgs :: forall a rnd bnch.
  (GenArgs (StateT (Maybe (SKOf a)) rnd) bnch, MonadRandom rnd,
   ShowType a, ResultOf bnch ~ Test a)
  => bnch -> Proxy a -> rnd TF.Test
hideArgs f p = do
  res <- evalStateT (genArgs f) (Nothing :: Maybe (SKOf a))
  case res of
    Test b -> return $ testProperty (showType p) b
    TestM b -> testProperty (showType p) <$> b

sheTests :: [TF.Test]
sheTests =
  [testGroupM "Dec . Enc"  $ applyDec decParams $ hideArgs prop_encDec,
   testGroupM "DecU . Enc" $ applyCTFunc ctParams $ hideArgs prop_encDecU,
   testGroupM "AddPub"     $ applyCTFunc ctParams $ hideArgs prop_addPub,
   testGroupM "MulPub"     $ applyCTFunc ctParams $ hideArgs prop_mulPub,
   testGroupM "ScalarPub"  $ applyCTFunc ctParams $ hideArgs prop_addScalar,
   testGroupM "CTAdd"      $ applyCTFunc ctParams $ hideArgs prop_ctadd,
   testGroupM "CTMul"      $ applyCTFunc ctParams $ hideArgs prop_ctmul,
   testGroupM "CT zero"    $ applyCTFunc ctParams $ hideArgs prop_ctzero,
   testGroupM "CT one"     $ applyCTFunc ctParams $ hideArgs prop_ctone,
   testGroupM "ModSwitch PT" modSwPTTests,
   testGroupM "Tunnel"       tunnelTests,
   testGroupM "Twace"      $ applyCTTwEm twoIdxParams $ hideArgs prop_cttwace,
   testGroupM "Embed"      $ applyCTTwEm twoIdxParams $ hideArgs prop_ctembed,
   testGroupM "KSLin"      $ applyKSQ ksqParams $ hideArgs prop_ksLin,
   testGroupM "keySwitch"  $ applyKSQ ksqParams $ hideArgs prop_ksQuad
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
ctParams :: Proxy CTParams
ctParams = Proxy

type DecParams = ( '(,) <$> Tensors) <*> (Nub (Filter Liftable CTCombos))
decParams :: Proxy DecParams
decParams = Proxy

type Zq'Params = ( '(,) <$> Tensors) <*> (Map AddZq (Filter NonLiftable MM'PQCombos))
type KSQParams = ( '(,) <$> Gadgets) <*> Zq'Params
ksqParams :: Proxy KSQParams
ksqParams = Proxy

type TwoIdxParams = ( '(,) <$> Tensors) <*> '[ '(F1, F7, F3, F21, Zq 2, Zq 18869761)]
twoIdxParams :: Proxy TwoIdxParams
twoIdxParams = Proxy

prop_ksLin :: (DecryptUCtx t m m' z zp zq, Eq (Cyc t m zp))
  => KSLinear t m m' z zp zq zq' gad
     -> PTCT m zp (Cyc t m' zq)
     -> Test '(t,m,m',zp,zq,zq',gad)
prop_ksLin (KSL kswlin skout) (PTCT x' x) =
  let y = kswlin x
      y' = decryptUnrestricted skout y
  in test $ x' == y'

prop_ksQuad :: (Ring (CT m zp (Cyc t m' zq)),
                DecryptUCtx t m m' z zp zq,
                Eq (Cyc t m zp))
  => SK (Cyc t m' z)
     -> KSHint m zp t m' zq gad zq'
     -> PTCT m zp (Cyc t m' zq)
     -> PTCT m zp (Cyc t m' zq)
     -> Test '(t,m,m',zp,zq,zq',gad)
prop_ksQuad sk (KeySwitch kswq) (PTCT y1 x1) (PTCT y2 x2) =
  let x' = kswq $ x1*x2
      y = y1*y2
      x = decryptUnrestricted sk x'
  in test $ y == x

prop_addPub :: forall t m m' z zp zq .
  (DecryptUCtx t m m' z zp zq,
   AddPublicCtx t m m' zp zq,
   Eq (Cyc t m zp))
  => SK (Cyc t m' z)
     -> Cyc t m zp
     -> PTCT m zp (Cyc t m' zq)
     -> Test '(t,m,m',zp,zq)
prop_addPub sk x (PTCT y' y) =
  let xy = addPublic x y
      xy' = decryptUnrestricted sk xy
  in test $ xy' == (x+y')

prop_mulPub :: (DecryptUCtx t m m' z zp zq,
                MulPublicCtx t m m' zp zq,
                Eq (Cyc t m zp))
  => SK (Cyc t m' z)
     -> Cyc t m zp
     -> PTCT m zp (Cyc t m' zq)
     -> Test '(t,m,m',zp,zq)
prop_mulPub sk x (PTCT y' y) =
  let xy = mulPublic x y
      xy' = decryptUnrestricted sk xy
  in test $ xy' == (x*y')

prop_addScalar :: (DecryptUCtx t m m' z zp zq,
                   AddScalarCtx t m' zp zq,
                   Eq (Cyc t m zp))
  => SK (Cyc t m' z) -> zp -> PTCT m zp (Cyc t m' zq) -> Test '(t,m,m',zp,zq)
prop_addScalar sk c (PTCT x' x) =
  let cx = addScalar c x
      cx' = decryptUnrestricted sk cx
  in test $ cx' == ((scalarCyc c)+x')

prop_ctadd :: (DecryptUCtx t m m' z zp zq,
               Additive (CT m zp (Cyc t m' zq)),
               Eq (Cyc t m zp))
  => SK (Cyc t m' z)
     -> PTCT m zp (Cyc t m' zq)
     -> PTCT m zp (Cyc t m' zq)
     -> Test '(t,m,m',zp,zq)
prop_ctadd sk (PTCT x1' x1) (PTCT x2' x2) =
  let y = x1+x2
      y' = decryptUnrestricted sk y
  in test $ x1'+x2' == y'

prop_ctmul :: (DecryptUCtx t m m' z zp zq,
               Ring (CT m zp (Cyc t m' zq)),
               Eq (Cyc t m zp))
  => SK (Cyc t m' z)
     -> PTCT m zp (Cyc t m' zq)
     -> PTCT m zp (Cyc t m' zq)
     -> Test '(t,m,m',zp,zq)
prop_ctmul sk (PTCT x1' x1) (PTCT x2' x2) =
  let y = x1*x2
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
  => SK (Cyc t r' z) -> PTCT r zp (Cyc t r' zq) -> Test '(t,r,r',s,s',zp,zq)
prop_ctembed sk (PTCT x' x) =
  let y = embedCT x :: CT s zp (Cyc t s' zq)
      y' = decryptUnrestricted (embedSK sk) y
  in test $ (embed x' :: Cyc t s zp) == y'

-- CT must be encrypted with key from small ring
prop_cttwace :: forall t r r' s s' z zp zq .
  (Eq zp,
   EncryptCtx t s s' z zp zq,
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

-- one-off tests, no hideArgsper
prop_modSwPT :: forall t m m' z zp zp' zq .
  (Eq zp, Eq zp',
   DecryptUCtx t m m' z zp zq,
   DecryptUCtx t m m' z zp' zq,
   ModSwitchPTCtx t m' zp zp' zq,
   RescaleCyc (Cyc t) zp zp',
   Ring (Cyc t m zp),
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

modSwPTTests :: [IO TF.Test]
modSwPTTests = (modSwPTTests' (Proxy::Proxy CT.CT)) ++ (modSwPTTests' (Proxy::Proxy RT))
 where modSwPTTests' p =
        [helper (hideArgs prop_modSwPT) p (Proxy::Proxy '(F7,F21,Zq 4,Zq 8,Zq 18869761)),
         helper (hideArgs prop_modSwPT) p (Proxy::Proxy '(F7,F42,Zq 2,Zq 4,Zq (18869761 ** 19393921)))]

tunnelTests :: [IO TF.Test]
tunnelTests = (tunnelTests' (Proxy::Proxy CT.CT)) ++ (tunnelTests' (Proxy::Proxy RT))
  where tunnelTests' p =
         [helper (hideArgs prop_ringTunnel) p
          (Proxy::Proxy '(F8,F40,F20,F60,Zq 4,Zq (18869761 ** 19393921),TrivGad))]

prop_ringTunnel :: forall t e r s e' r' s' z zp zq gad .
  (TunnelCtx t e r s e' r' s' z zp zq gad,
   EncryptCtx t r r' z zp zq,
   GenSKCtx t r' z Double,
   GenSKCtx t s' z Double,
   DecryptUCtx t s s' z zp zq,
   Random zp, Eq zp,
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

