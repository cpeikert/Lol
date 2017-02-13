{-|
Module      : Crypto.Lol.Compiler.CT
Description : Basic ciphertext AST.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Currently unmaintained.
-}

{-# LANGUAGE TypeOperators, RankNTypes, KindSignatures,
             DataKinds, TypeFamilies, GADTs, FlexibleContexts,
             ScopedTypeVariables, FlexibleInstances,
             UndecidableInstances, InstanceSigs, MultiParamTypeClasses,
             RebindableSyntax, ConstraintKinds #-}

-- | A module containing deep and shallow embeddings for CT operations
-- Some of these nodes require auxillary information (key switch hints)
-- so a simple compiler is provided in CTCompiler and CTDummy

module Crypto.Lol.Compiler.CT where

import Control.DeepSeq

import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Cyc
import Crypto.Lol.Cyclotomic.RescaleCyc
import Crypto.Lol.Gadget
import Crypto.Lol.Prelude hiding (drop, lookup, (!!))
import Crypto.Lol.Applications.SymmSHE

import Language.Syntactic
import Language.Syntactic.Functional
import Data.Typeable

type KeyID = Int

data CTOps :: (* -> *) where
  AddPublic :: (r'q ~ Cyc t m' zq, AddPublicCtx t m m' zp zq, Show (Cyc t m zp), NFData zp, NFData (CRTExt zp))
            => Cyc t m zp -> CTOps (CT m zp r'q :-> Full (CT m zp r'q))

  MulPublic :: (r'q ~ Cyc t m' zq, MulPublicCtx t m m' zp zq, Show (Cyc t m zp), NFData zp, NFData (CRTExt zp))
            => Cyc t m zp -> CTOps (CT m zp r'q :-> Full (CT m zp r'q))

  KeySwQuad :: (CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq))
               -> CTOps (CT m zp (Cyc t m' zq) :-> Full (CT m zp (Cyc t m' zq)))

  ModSwitchPT :: (ModSwitchPTCtx t m' zp zp' zq,
                  -- the next constraints are only needed to safisfy the recursive
                  -- constraints when mapping. FIXME!!!! (a problem due to hidden types)
                  Typeable (Cyc t m' (LiftOf zp)),
                  ToInteger (LiftOf zp), CElt t (LiftOf zp))
              => CTOps (CT m zp (Cyc t m' zq) :-> Full (CT m zp' (Cyc t m' zq)))

  ModSwitchCT :: (RescaleCyc (Cyc t) zq zq', ToSDCtx t m' zp zq, CElt t (DecompOf zq),
                  -- these are needed to satisfy CTArgsCtx in KeyInferPass
                  -- I think it's because I assumed that the constraints are similar
                  -- for all nodes, which is certainly the case, but we clearly need
                  -- a cleaner way.
                  ToInteger (DecompOf zq), Typeable (DecompOf zq))
              => CTOps (CT m zp (Cyc t m' zq) :-> Full (CT m zp (Cyc t m' zq')))

  CTRingTunn :: (CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' zq))
               -> CTOps (CT r zp (Cyc t r' zq) :-> Full (CT s zp (Cyc t s' zq)))

-- | adds an unencrypted (public) value to a ciphertext
addPublicCT :: (AddPublicCtx t m m' zp zq, NFData zp, NFData (CRTExt zp), CTOps :<: dom',
                dom ~ Typed dom', Show (Cyc t m zp), Typeable (CT m zp (Cyc t m' zq)))
  => Cyc t m zp -> ASTF dom (CT m zp (Cyc t m' zq)) -> ASTF dom (CT m zp (Cyc t m' zq))
addPublicCT c = (injT (AddPublic c) :$)

mulPublicCT :: (MulPublicCtx t m m' zp zq, NFData zp, NFData (CRTExt zp), CTOps :<: dom',
                dom ~ Typed dom', Show (Cyc t m zp), Typeable (CT m zp (Cyc t m' zq)))
  => Cyc t m zp -> ASTF dom (CT m zp (Cyc t m' zq)) -> ASTF dom (CT m zp (Cyc t m' zq))
mulPublicCT c = (injT (MulPublic c) :$)

type ASTRoundPTCtx dom dom' t m m' zp zp' zq =
  (CTOps :<: dom', dom ~ Typed dom', ModSwitchPTCtx t m' zp zp' zq,
   ToInteger (LiftOf zp), CElt t (LiftOf zp),
   Typeable (Cyc t m' (LiftOf zp)), Typeable (CT m zp' (Cyc t m' zq)), m `Divides` m')

-- | switches the modulus of an encrypted plaintext
roundPT :: (ASTRoundPTCtx dom dom' t m m' zp zp' zq)
  => ASTF dom (CT m zp (Cyc t m' zq)) -> ASTF dom (CT m zp' (Cyc t m' zq))
roundPT = (injT ModSwitchPT :$)

type ASTRoundCTCtx dom dom' t m m' zp zq zq' =
  (CTOps :<: dom', dom ~ Typed dom', RescaleCyc (Cyc t) zq zq', ToSDCtx t m' zp zq,
   CElt t (DecompOf zq),
   ToInteger (DecompOf zq), Typeable (DecompOf zq), Typeable (CT m zp (Cyc t m' zq')), m `Divides` m')

-- | switches the modulus of a ciphertext
roundCT :: (ASTRoundCTCtx dom dom' t m m' zp zq zq')
  => ASTF dom (CT m zp (Cyc t m' zq)) -> ASTF dom (CT m zp (Cyc t m' zq'))
roundCT = (injT ModSwitchCT :$)

instance StringTree CTOps
instance EvalEnv CTOps env

instance NFData1 CTOps where
  rnf1 (AddPublic p) = rnf p
  rnf1 (MulPublic p) = rnf p
  rnf1 (KeySwQuad f) = f `seq` rnf f
  rnf1 ModSwitchPT = ()
  rnf1 ModSwitchCT = ()
  rnf1 (CTRingTunn f) = f `seq` rnf f

instance Symbol CTOps where
  symSig (AddPublic _) = signature
  symSig (MulPublic _) = signature
  symSig (KeySwQuad _) = signature
  symSig ModSwitchPT = signature
  symSig ModSwitchCT = signature
  symSig (CTRingTunn _) = signature

instance Render CTOps where
  renderSym (AddPublic a) = "+>" ++ (show a)
  renderSym (MulPublic a) = "*>" ++ (show a)
  renderSym (KeySwQuad _) = "keySwitchQuad"
  renderSym ModSwitchPT = "ptModSwitch"
  renderSym ModSwitchCT = "ctModSwitch"
  renderSym (CTRingTunn _) = "tunnel"

instance Eval CTOps where
  evalSym (AddPublic c) = addPublic c
  evalSym (MulPublic c) = mulPublic c
  evalSym (KeySwQuad f) = f
  evalSym ModSwitchPT = modSwitchPT
  evalSym ModSwitchCT = rescaleLinearCT
  evalSym (CTRingTunn f) = f
