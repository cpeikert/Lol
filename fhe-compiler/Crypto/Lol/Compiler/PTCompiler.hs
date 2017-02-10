{-# LANGUAGE TypeOperators, ConstraintKinds, FlexibleContexts, GADTs, TypeFamilies #-}

module Crypto.Lol.Compiler.PTCompiler where

import Data.Typeable
import Data.Constraint
import Control.DeepSeq



import Language.Syntactic hiding (size)

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE as SHE
import Crypto.Lol.Compiler.CT
import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc

import Control.Monad.Random

import Types

type ZqCtx t zp zq zqup = (
  CElt t zqup,
  --CRTEmbed zqup, 
  --Eq zqup,
  --Random zqup,
  --TElt t zqup,
  --CRTrans zqup,
  --IntegralDomain zqup,
  --ZeroTestable zqup, 
  --NFData zqup,
  --TElt t (CRTExt zqup),
  --CRTrans (CRTExt zqup), 
  --IntegralDomain (CRTExt zqup),
  --ZeroTestable (CRTExt zqup),
  --NFData (CRTExt zqup), 

  --RElt t (DecompOf zqup),
  TElt t (DecompOf zqup),
  CRTrans (DecompOf zqup),
  NFData (DecompOf zqup),
  Random (DecompOf zqup),



  RElt t (CRTExt (DecompOf zqup)),
  --TElt t (CRTExt (DecompOf zqup)),
  --CRTrans (CRTExt (DecompOf zqup)),
  --IntegralDomain (CRTExt (DecompOf zqup)),
  --ZeroTestable (CRTExt (DecompOf zqup)),
  --NFData (CRTExt (DecompOf zqup))


  CRTEmbed (DecompOf zqup),
  RescaleCyc (Cyc t) zqup zq,   
  Encode zp zqup,
  ToInteger (DecompOf zqup),
  Typeable (DecompOf zqup)
   
  )

data ModConstraints t zp zq where
  ZqNode :: Dict (ZqCtx t zp zq (ZQUp zq)) 
    -> ModConstraints t zp (ZQDown zq) 
    -> ModConstraints t zp (ZQUp zq) 
    -> ModConstraints t zp zq

data PTModSw :: (* -> *) where
  PTKSQ :: (Fact m, z ~ LiftOf zp, Tensor t,
            Typeable MyGad, Typeable t, Typeable z,
            ToInteger z, CElt t z)
    => PTModSw (Cyc t m zp :-> Full (Cyc t m zp))

foo :: (Fact m') => 
  ModConstraints t zp zq 
  -> PTModSw (Cyc t m zp 
                :-> Full (Cyc t m zp)) 
  -> Args (AST CTOps) (SHE.CT m zp (Cyc t m' (ZQUp zq)) 
                         :-> Full (SHE.CT m zp (Cyc t m' zq))) 
  -> ASTF CTOps (SHE.CT m zp (Cyc t m' zq))
foo (ZqNode Dict _ _) PTKSQ (arg :* Nil) = (inj $ ModSwitchCT) :$ arg
{-
bar :: ModConstraints t zp zq 
    -> ASTF (PTModSw :+: CTOps) 
        (Cyc t m zp :-> Full (Cyc t m'' zp'))
    -> ASTF (PTModSw :+: CTOps) 
        (SHE.CT m zp (Cyc t m' zq) :-> Full (SHE.CT m'' zp' (Cyc t m''' zq')))
bar 
-}
{-
type ASTRoundCTCtx dom dom' t m m' zp zq zq' = 
  (CTOps :<: dom', dom ~ Typed dom', RescaleCyc (Cyc t) zq zq', ToSDCtx t m' zp zq,
   CElt t (DecompOf zq), 
   ToInteger (DecompOf zq), Typeable (DecompOf zq), Typeable (CT m zp (Cyc t m' zq')), m `Divides` m')

data CTOps :: (* -> *) where
  ModSwitchCT :: (RescaleCyc (Cyc t) zq zq', ToSDCtx t m' zp zq, CElt t (DecompOf zq), 
                  ToInteger (DecompOf zq), Typeable (DecompOf zq))
              => CTOps (CT m zp (Cyc t m' zq) :-> Full (CT m zp (Cyc t m' zq')))
-}