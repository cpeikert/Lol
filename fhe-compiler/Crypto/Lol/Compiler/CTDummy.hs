{-|
Module      : Crypto.Lol.Compiler.CTDummy
Description : Dummy ciphertext AST.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Currently unmaintained.
-}

{-# LANGUAGE RebindableSyntax, RankNTypes, TypeOperators,
             ScopedTypeVariables, GADTs, KindSignatures, FlexibleInstances,
             MultiParamTypeClasses, UndecidableInstances, FlexibleContexts,
             ConstraintKinds, DeriveGeneric, DeriveDataTypeable, DataKinds #-}

-- | This module contains shallow and deep embeddings for dummy CT operations
-- It is recommended that these are used when constructing an AST of type CT,
-- together with the CT compiler in CTCompiler.hs

-- The nodes in this module *cannot* be evaluated,
-- they must be compiled to CT nodes first.

module Crypto.Lol.Compiler.CTDummy
(CTDummyOps(..)
,tunnDummy
,ksqDummy
,ASTTunnelCtx
,KSDummyCtx) where


import Control.DeepSeq
import Control.Monad.Identity

import Crypto.Lol.Cyclotomic.Cyc
import Crypto.Lol.Types.ZPP
import Crypto.Lol.Prelude hiding (lookup)
import Crypto.Lol.Applications.SymmSHE

import Data.Dynamic
import Language.Syntactic
import Language.Syntactic.Functional hiding (Let)

type KSDummyCtx z gad zq' m zp c m' zq =
  (Fact m, Fact m',
   z ~ LiftOf zp,

   KeySwitchCtx gad c m' zp zq zq',
   KSHintCtx gad c m' z zq',
   NFData zq',

   Typeable (Cyc c m' z),
   Typeable (CT m zp (Cyc c m' zq)),
   Typeable gad, Typeable zq')


data CTDummyOps :: (* -> *) where

  KeySwQDummy :: (KSDummyCtx z gad zq' m zp c m' zq)
    => proxy gad -> proxy zq'
       -> CTDummyOps (CT m zp (Cyc c m' zq) :-> Full (CT m zp (Cyc c m' zq)))

  --http://stackoverflow.com/questions/28870198/polykinds-in-gadt-constructors
  TunnDummy ::
    (TunnelCtx c e r s e' r' s' z zp zq gad, e ~ FGCD r s, Fact r,
     Typeable c, Typeable r', Typeable s', Typeable z, Typeable s, Typeable r,
     Typeable gad, Typeable zq, Typeable zp, CElt c (ZpOf zp),
     ZPP zp)
           => proxy gad -> CTDummyOps (CT r zp (Cyc c r' zq) :-> Full (CT s zp (Cyc c s' zq)))

  deriving (Typeable)

instance StringTree CTDummyOps
instance EvalEnv CTDummyOps env

instance NFData1 CTDummyOps where
  rnf1 (KeySwQDummy _ _) = ()
  rnf1 (TunnDummy _) = ()

instance Symbol CTDummyOps where
  symSig (KeySwQDummy _ _) = signature
  symSig (TunnDummy _) = signature

instance Render CTDummyOps where
  renderSym (KeySwQDummy _ _) = "keySwQuadDummy"
  renderSym (TunnDummy _) = "tunnDummy"

-- no eval instance for CTDummyOps
instance Eval CTDummyOps where
  evalSym = error "cannot evaluate CTDummyOps nodes!"

ksqDummy :: forall dom dom' gad c m m' z zp zq zq' .
  (CTDummyOps :<: dom', dom ~ Typed dom', KSDummyCtx z gad zq' m zp c m' zq)
  => ASTF dom (CT m zp (Cyc c m' zq)) -> Tagged '(gad, zq') (ASTF dom (CT m zp (Cyc c m' zq)))
ksqDummy = return . (injT (KeySwQDummy (Proxy::Proxy gad) (Proxy::Proxy zq')) :$)

type ASTTunnelCtx dom dom' gad c r r' s s' e e' z zp zq =
  (CTDummyOps :<: dom', dom ~ Typed dom', TunnelCtx c e r s e' r' s' z zp zq gad, e ~ FGCD r s,
   Fact r, Typeable c, Typeable r', Typeable s', Typeable z, Typeable s, Typeable r,
   Typeable gad, Typeable zq, Typeable zp, CElt c (ZpOf zp), ZPP zp)

tunnDummy :: forall dom dom' gad c r r' s s' e e' z zp zq .
  (ASTTunnelCtx dom dom' gad c r r' s s' e e' z zp zq)
  => ASTF dom (CT r zp (Cyc c r' zq)) -> Tagged gad (ASTF dom (CT s zp (Cyc c s' zq)))
tunnDummy = return . (injT (TunnDummy (Proxy::Proxy gad)) :$)
