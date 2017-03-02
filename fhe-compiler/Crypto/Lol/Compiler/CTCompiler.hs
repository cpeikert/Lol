{-|
Module      : Crypto.Lol.Compiler.CTCompiler
Description : Compiles plaintext AST to ciphertext AST.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Currently unmaintained.
-}

{-# LANGUAGE RebindableSyntax, FlexibleContexts,
             FlexibleInstances, DataKinds, GADTs, KindSignatures, MultiParamTypeClasses,
             ConstraintKinds, ScopedTypeVariables, RankNTypes, UndecidableInstances,
             TypeFamilies, DeriveDataTypeable, StandaloneDeriving, TypeOperators, UndecidableSuperClasses #-}

-- A module to compile an AST containing CTDummy nodes to an evaluatable AST
-- containing CT nodes.
-- Highly experimental, use at your own risk.

-- Compiler steps:
-- 1. genKeys
-- 2. genHints
-- 3. encryptInput
-- 4. getDecryptKey

module Crypto.Lol.Compiler.CTCompiler
(showIDDecor
,genKeys
,genHints
,encryptInput
,getDecryptKey
,IDDecor
) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Identity

import Crypto.Lol.Compiler.AST
import Crypto.Lol.Compiler.CT
import Crypto.Lol.Compiler.CTDummy
import Crypto.Lol.Cyclotomic.Cyc
import Crypto.Lol.Cyclotomic.Linear
import Crypto.Lol.Prelude hiding (lookup, lift)
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Types.ZPP

import Data.Coerce
import Data.Constraint hiding (Sub)
import Data.Dynamic
import Data.Map as M (Map, lookup, empty, insert, elems, delete)
import Data.Maybe
import Language.Syntactic
import Language.Syntactic.Functional hiding (Let)

-- this module passes over an AST and decorates
-- it with abstract key IDs representing the
-- decryption key for a particular node

-- decoration wrappers

newtype IDDecor a = ID {unKeyID :: (KeyID, Int)} deriving (Typeable)
instance NFData1 IDDecor where
  rnf1 (ID p) = rnf p

type InferDoms sym = (
  CTDummyOps :<: sym, CTOps :<: sym, RING :<: sym,
  ADDITIVE :<: sym, BindingT :<: sym, Let :<: sym)

showIDDecor :: IDDecor a -> String
showIDDecor (ID a) = show a

-- | Decorates AST nodes with a (KeyID,Int) where the KeyID maps to the dynamic key that decrypts that node
-- and the Int is degree of the ciphertext in the key
-- Fails if KeySwQDummy nodes don't have degree 2, and assumes that all VarT nodes have degree 1

genKeys :: (InferDoms sym, ToRational v, NFData v, MonadRandom mon,
            Typeable sym, BUT (Typed sym) (Typed sym :&: IDDecor) a)
  => v -> ASTF (Typed sym) a -> mon (ASTF (Typed sym :&: IDDecor) a, Map TypeRep (Int,Dynamic))
genKeys v ast = flip runStateT (M.empty :: Map TypeRep (Int,Dynamic)) (bottomUpMapMCT (genKeysWrapper v) ast)

genKeysWrapper :: (InferDoms sym, ToRational v, NFData v, MonadRandom mon, Typeable sym,
  MonadState (Map TypeRep (Int, Dynamic)) mon)
  => v -> (Typed sym) sig -> Args (AST (Typed sym :&: IDDecor)) sig -> StateT Env mon (ASTF (Typed sym :&: IDDecor) (DenResult sig))
genKeysWrapper v node args = do
  info <- genKeys' v node args
  return $ appArgs (Sym (node :&: info)) args

genKeys' :: forall sym v mon sig . (InferDoms sym, ToRational v, NFData v, MonadRandom mon, Typeable sym,
  MonadState (Map TypeRep (Int, Dynamic)) mon)
  => v -> (Typed sym) sig -> Args (AST (Typed sym :&: IDDecor)) sig -> StateT Env mon (IDDecor (DenResult sig))
genKeys' v node (arg1 :* arg2 :* Nil)
  | Just Mul <- prj node,
    (kid1, p1) <- unKeyID $ getDecor arg1,
    (kid2, p2) <- unKeyID $ getDecor arg2,
    kid1 == kid2 = return $ ID (kid1, p1+p2)
  | Just Let <- prj node = return $ coerce $ getDecor arg2
genKeys' v node (arg :* Nil)
  | Just (LamT v) <- prj node = return $ coerce $ getDecor arg
  | Just (KeySwQDummy _ _) <- prj node,
    (kid, 2) <- unKeyID $ getDecor arg = return $ ID (kid, 1)
  | Just (TunnDummy _) <- prj node,
    (_ :: (Typed sym) (CT r zp (Cyc t r' zq) :-> Full (CT s zp (Cyc t s' zq)))) <- node = do
    kid <- lift $ genKeyIfNotExists (Proxy::Proxy (Cyc t s' (LiftOf zp))) v
    return $ ID (kid, 1)
genKeys' v node Nil
  | Just (VarT name) <- prj node = do
    env <- get
    case lookup name env of
     Just (Hidden (Proxy::Proxy (sim (CT m zp (Cyc t m' zq)), sim' (CT m zp (Cyc t m' zq)))) dict) ->
      case eqT of
       Just (Refl :: (Proxy (sim (CT m zp (Cyc t m' zq)), sim' (CT m zp (Cyc t m' zq))) :~: (Proxy ((Typed sym) (CT m zp (Cyc t m' zq)), (Typed sym :&: IDDecor) (CT m zp (Cyc t m' zq)))))) ->
        case dict of
         Dict -> do
          kid <- lift $ genKeyIfNotExists (Proxy::Proxy (Cyc t m' (LiftOf zp))) v
          return $ ID (kid, 1)
-- it's not safe to write a "generic" case here, because someone
-- could add more domains later for which the generic case is incorrect
-- (i.e. a node that takes one arg but modifies the secret key)
genKeys' _ node (arg :* Nil)
  | Just (AddPublic _) <- prj node = return $ coerce $ getDecor arg
  | Just (MulPublic _) <- prj node = return $ coerce $ getDecor arg
  | Just ModSwitchPT <- prj node = return $ coerce $ getDecor arg
  | Just ModSwitchCT <- prj node = return $ coerce $ getDecor arg
genKeys' _ node (arg :* _ :* Nil)
  | Just Add <- prj node = return $ coerce $ getDecor arg
  | Just Sub <- prj node = return $ coerce $ getDecor arg



genKeyIfNotExists :: forall t c m z v mon .
  (MonadState (Map TypeRep (Int, Dynamic)) mon, Typeable (Cyc t m z), Fact m,
   MonadRandom mon, ToRational v, NFData v, CElt t z, ToInteger z, CElt t Double)
  => Proxy (Cyc t m z) -> v -> mon KeyID
genKeyIfNotExists _ v = do
  let tr = typeRep (Proxy::Proxy (Cyc t m z))
  res <- gets $ lookup tr
  case res of
    Just (i,_) -> return i -- key is already gen'd, just to a bottom up recursive call
    Nothing -> do
      nextID <- getNextID
      sk :: SK (Cyc t m z) <- genSK v
      modify (insert tr (nextID, toDyn sk))
      return nextID

getNextID :: (MonadState (Map TypeRep (Int, Dynamic)) m) => m Int
getNextID = do
  ids <- liftM (map fst) $ gets elems
  return $ if null ids then 1 else 1 + (maximum ids)









-- | Takes a key map and a plaintext and returns a ciphertext encrypting the plaintext
-- We assume that there is exactly one key for each plaintext type, and each plaintext
-- is encrypted as a linear ciphertext, since we assume that all VarT nodes are linear
-- ciphertexts
encryptInput :: forall t m zp m' zq mon z .
  (EncryptCtx t m m' z zp zq, MonadRandom mon, z ~ LiftOf zp, Typeable (Cyc t m' z))
  => Map TypeRep (Int,Dynamic) -> Cyc t m zp -> mon (CT m zp (Cyc t m' zq), SK (Cyc t m' z))
encryptInput keyMap pt =
  let sk :: SK (Cyc t m' z) = fromMaybe (error "key not found!") $ fromDynamic . snd =<< lookup (typeRep (Proxy::Proxy (Cyc t m' z))) keyMap
  in liftM (\z -> (z,sk)) $ encrypt sk pt

-- | returns the decryption key corresponding to the ciphertext, raised to the appropriate power
getDecryptKey :: forall sym r zp t r' zq s zp' s' zq' z .
  (CElt t z, Fact s', Typeable (Cyc t s' z), z ~ LiftOf zp')
  => Map KeyID Dynamic
     -> ASTF (sym :&: IDDecor) (CT r zp (Cyc t r' zq) -> CT s zp' (Cyc t s' zq'))
     -> SK (Cyc t s' z)
getDecryptKey keyMap node =
  let (kid,pow) = unKeyID $ getDecor node
      s :: SK (Cyc t s' z) = fromMaybe (error "key not found!") $ fromDynamic =<< lookup kid keyMap
  in if pow == 1 then s else error "only linear decryption keys supported in Compiler"

type HintEnv = Map ((KeyID, Int), (KeyID,Int)) Dynamic

hintLookup :: (MonadState HintEnv m, Typeable gad, Typeable zq,
               Typeable (CT r zp ((c :: Factored -> * -> *) r' zq)), Typeable (CT s zp (c s' zq)))
  => (KeyID, Int) -> (KeyID, Int)
     -> m (Maybe (Tagged gad (CT r zp (c r' zq) -> CT s zp (c s' zq))))
hintLookup kids1 kids2 = liftM (fromDynamic <=< lookup (kids1, kids2)) get


-- | The main component of the compiler. Generates auxillary information necessary to convert CTDummy
-- nodes to evaulatable CT nodes.
genHints :: forall v sym a m .
  (CTOps :<: sym, MonadRandom m, CTDummyOps :<: sym)
  => Map KeyID Dynamic -> ASTF (Typed sym :&: IDDecor) a -> m (ASTF (Typed sym :&: IDDecor) a)
genHints keyMap = flip evalStateT (M.empty :: HintEnv) . bottomUpMapM (genHints' keyMap)

genHints' ::
  (CTDummyOps :<: sym, CTOps :<: sym,
   MonadRandom mon, MonadState HintEnv mon)
  => Map KeyID Dynamic
     -> (Typed sym :&: IDDecor) sig
     -> Args (AST (Typed sym :&: IDDecor)) sig
     -> mon ((Typed sym :&: IDDecor) sig)
genHints' keyMap (node :&: info) _
  | Just (KeySwQDummy (_ :: p gad) (_ :: p zq')) <- prj node,
    (kid, _) <- unKeyID info,
    (_ :: (Typed sym) (CT m zp (Cyc t m' zq) :-> Full (CT m zp (Cyc t m' zq)))) <- node = do
      thint <- maybeGenQuadHint keyMap kid
      return $ (Typed $ inj $ KeySwQuad (proxy thint (Proxy::Proxy (gad,zq')))) :&: info
-- this does not memoize the linear keys right now
genHints' keyMap (node :&: info) (arg :* Nil)
  | Just (TunnDummy (_::p0 gad)) <- prj node,
    (koutid, _) <- unKeyID info,
    (kinid, _) <- unKeyID $ getDecor arg,
    (_ :: (Typed sym) (CT r zp (Cyc t r' zq) :-> Full (CT s zp (Cyc t s' zq)))) <- node = do
      thint <- maybeGenLinHint keyMap kinid koutid
      return $ (Typed $ inj $ CTRingTunn (proxy thint (Proxy::Proxy gad))) :&: info
genHints' _ node _ = return node


swapTag :: (Monad m) => TaggedT s m a -> m (Tagged s a)
swapTag = liftM tag . untagT

-- a helper function that either returns a hint from the environment if available,
-- or generates a new hint otherwise.
maybeGenQuadHint :: forall mon gad zq' m zp t m' zq z .
  (MonadState HintEnv mon,
   MonadRandom mon,
   z ~ LiftOf zp,

   KeySwitchCtx gad t m' zp zq zq',
   KSHintCtx gad t m' z zq',
   NFData zq',
   Typeable (Cyc t m' z),
   Typeable (CT m zp (Cyc t m' zq)),
   Typeable gad, Typeable zq')
  => Map KeyID Dynamic
     -> KeyID
     -> mon (Tagged (gad,zq') (CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)))
maybeGenQuadHint keyMap kid = do
  --mhint <- hintLookup (kid, 2) (kid, 1) -- look for a quadratic hint for key with id kid
  --case mhint of
  --  Just hint -> return hint -- we have already gen'd a hint from kid1 to kid2
  --  Nothing -> do
      let sk :: SK (Cyc t m' z) = fromMaybe (error "key not found!") $ fromDynamic =<< lookup kid keyMap  -- get first key
      h <- swapTag $ keySwitchQuadCirc sk
      h `seq` modify $ insert ((kid,2),(kid,1)) $ toDyn h -- and add it to the environment
      return h


maybeGenLinHint :: forall mon gad r s zp t r' s' zq z e e' .
  (MonadState HintEnv mon,
   MonadRandom mon,
   z ~ LiftOf zp,

   TunnelCtx t e r s e' r' s' z zp zq gad,
   e ~ FGCD r s,
   ZPP zp,
   CElt t (ZpOf zp),
   Typeable (Cyc t r' z),
   Typeable (Cyc t s' z),
   Typeable (CT r zp (Cyc t r' zq)),
   Typeable (CT s zp (Cyc t s' zq)),
   Typeable gad, Typeable zq)
  => Map KeyID Dynamic
     -> KeyID
     -> KeyID
     -> mon (Tagged gad (CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' zq)))
maybeGenLinHint keyMap kid1 kid2 = (do
  mhint <- hintLookup (kid1, 1) (kid2, 1) -- look for a linear hint for key with id kid
  case mhint of
    Just hint -> return hint -- we have already gen'd a hint from kid1 to kid2
    Nothing -> do
      let sk1 :: SK (Cyc t r' z) = fromMaybe (error "key1 not found!") $ fromDynamic =<< lookup kid1 keyMap  -- get first key
          sk2 :: SK (Cyc t s' z) = fromMaybe (error "key1 not found!") $ fromDynamic =<< lookup kid2 keyMap  -- get first key
          crts = proxy crtSet (Proxy::Proxy e)
          r = proxy totientFact (Proxy::Proxy r)
          e = proxy totientFact (Proxy::Proxy e)
          dim = r `div` e
          -- only take as many crts as we need
          -- otherwise linearDec fails
          linf = linearDec (take dim crts) :: Linear t zp e r s
      h <- swapTag $ tunnelCT linf sk2 sk1
      h `seq` modify $ insert ((kid1,1),(kid2,1)) $ toDyn h -- and add it to the environment
      return h) \\ gcdDivides (Proxy :: Proxy r) (Proxy :: Proxy s)








































bottomUpMapMCT :: (Monad m, BUT sym sym' a)
  => ( forall b sig . (BUTCtx b, b ~ DenResult sig) =>
            sym sig -> Args (AST sym') sig -> StateT Env m (ASTF sym' b)
     )
     -> ASTF sym a
     -> m (ASTF sym' a)
bottomUpMapMCT f = (`evalStateT` M.empty) . (bottomUpMapM'' f)

type Env = Map Name HiddenType

data HiddenType where
  Hidden :: (Typeable a, Typeable sym,  Typeable sym', Fact m, Fact m', a ~ CT m zp (Cyc t m' zq), CElt t Double)
         => Proxy (sym a, sym' a) -> Dict (BUT sym sym' a) -> HiddenType

instance Show HiddenType where
  show (Hidden h d) = show $ typeRep h

instance Eq HiddenType where
  (Hidden p1 _) == (Hidden p2 _) = (typeRep p1) == (typeRep p2)

-- 1. Make instances for each domain rather than forcing one specific domain
-- 2. Make the constraint a parameter. This will be hard because we won't know if a
--    param constraint is satsified for recursive calls
-- 3. see bottomUpMapM for an *unconstrained* version of this function

-- superclass constraint makes the Hidden type a little simpler
class (BUTCtx a) => BUT sym sym' a where
  type BUTCtx a :: Constraint

  bottomUpMapM'' ::
    (MonadState Env m)
    => ( forall b sig . (BUTCtx b, b ~ DenResult sig) =>
            sym sig -> Args (AST sym') sig -> m (ASTF sym' b)
        )
     -> ASTF sym a
     -> m (ASTF sym' a)

type family UnType dom where
  UnType (dom :&: info) = (UnType dom) :&: info
  UnType (Typed dom) = dom
  UnType dom = dom

instance (InferDoms (UnType sym), sym ~ Typed s, Typeable sym, Typeable sym', BUTCtx (CT m zp (Cyc t m' zq)))
  => BUT sym sym' (CT m zp (Cyc t m' zq)) where

  type BUTCtx (CT m zp (Cyc t m' zq)) =
    (Fact m, Fact m', CElt t (LiftOf zp), ToInteger (LiftOf zp),
     Typeable (Cyc t m' (LiftOf zp)), CElt t Double)

  bottomUpMapM'' f ((Sym node) :$ arg)
    | Just (KeySwQDummy _ _) <- prj node = oneArgBU f node arg
    | Just ModSwitchPT <- prj node = oneArgBU f node arg
    | Just ModSwitchCT <- prj node = oneArgBU f node arg
    | Just (TunnDummy _) <- prj node = oneArgBU f node arg
    | Just (AddPublic _) <- prj node = oneArgBU f node arg
    | Just (MulPublic _) <- prj node = oneArgBU f node arg
  bottomUpMapM'' f ((Sym node) :$ arg1 :$ arg2)
    | Just Add <- prj node = twoArgBU f node arg1 arg2
    | Just Sub <- prj node = twoArgBU f node arg1 arg2
    | Just Mul <- prj node = twoArgBU f node arg1 arg2
  bottomUpMapM'' f (Sym node)
    | Just (VarT v) <- prj node = do
      env <- get
      case lookup v env of
        Just _ -> return ()
        -- insert a new variable into the environment
        Nothing -> modify $ insert v
          (Hidden (Proxy::Proxy (sym  (CT m zp (Cyc t m' zq)),
                                 sym' (CT m zp (Cyc t m' zq)))) Dict)
      f node Nil
  bottomUpMapM'' f ((Sym node) :$ bind :$ (lam@(lamt :$ _)))
    | Just Let <- prj node,
      (_ :: ASTF sym d) <- bind,
      Just (LamT v) <- prj lamt = do
      lam' :: ASTF sym' (d -> CT m zp (Cyc t m' zq)) <- bottomUpMapM'' f lam
      env <- get
      -- we expect to find v in the env of the recursive call, otherwise
      -- this LamT binds a variable that is never used
      case lookup v env of
       Just (Hidden (Proxy::Proxy (sim d', sim' d')) d') -> case eqT of
        Just (Refl :: (Proxy (sim d', sim' d') :~: (Proxy (sym d, sym' d)))) -> case d' of
         Dict -> do
           -- env probably contains v
           -- we need to remove v before converting the binding
           -- because v is not in scope in the binding
           -- (also, it needs to be removed to satisfy the contract of the Environment
           --  when we return: since v is bound by the let, v shouldn't be in the
           --  returned environment)
           modify $ delete v
           bind' <- bottomUpMapM'' f bind
           f node (bind' :* lam' :* Nil)

instance (InferDoms (UnType sym), sym ~ Typed s, Typeable sym, Typeable sym', BUT sym sym' b)
  => BUT sym sym' (a -> b) where

  type BUTCtx (a -> b) = BUTCtx b

  bottomUpMapM'' f ((Sym node) :$ arg)
    | Just (LamT _) <- prj node = oneArgBU f node arg

oneArgBU :: (BUT sym sym' b, BUTCtx a, MonadState Env m)
    => ( forall d sig . (BUTCtx d, d ~ DenResult sig) =>
            sym sig -> Args (AST sym') sig -> m (ASTF sym' d)
        )
       -> sym (b :-> Full a)
       -> ASTF sym b
       -> m (ASTF sym' a)
oneArgBU f node arg = do
  arg' <- bottomUpMapM'' f arg
  f node (arg' :* Nil)

twoArgBU :: (BUT sym sym' b, BUT sym sym' c, BUTCtx a, MonadState Env m)
    => ( forall d sig . (BUTCtx d, d ~ DenResult sig) =>
            sym sig -> Args (AST sym') sig -> m (ASTF sym' d)
        )
       -> sym (c :-> b :-> Full a)
       -> ASTF sym c
       -> ASTF sym b
       -> m (ASTF sym' a)
twoArgBU f node arg1 arg2 = do
  arg1' <- bottomUpMapM'' f arg1
  arg2' <- bottomUpMapM'' f arg2
  f node (arg1' :* arg2' :* Nil)
