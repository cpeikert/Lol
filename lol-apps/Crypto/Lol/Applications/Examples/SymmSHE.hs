{-|
Module      : Crypto.Lol.Applications.Examples.SymmSHE
Description : Example using SymmSHE.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example using SymmSHE.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Examples.SymmSHE (sheMain) where

import Crypto.Lol                      hiding ((^))
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Types

import Algebra.Ring ((^))

import Control.Monad.Random (getRandom)

-- PTIndex must divide CTIndex
type PTIndex = F128

-- Crypto.Lol includes Factored types F1..F512
-- for cyclotomic indices outside this range,
-- we provide a TH wrapper.
-- TH to constuct the cyclotomic index 11648
type CTIndex = $(fType $ 2^7 * 7 * 13)

-- To use crtSet (for example, when ring switching), the plaintext
-- modulus must be a PrimePower (ZPP constraint).  Crypto.Lol exports
-- (via Crypto.Lol.Factored) PP2,PP4,...,PP128, as well as some prime
-- powers for 3,5,7, and 11.  Alternately, an arbitrary prime power
-- p^e can be constructed with the Template Haskell splice $(ppType
-- (p,e)).  For applications that don't use crtSet, the PT modulus can
-- be a TypeLit.
type PTZq = ZqBasic PP8 Int64

-- uses GHC.TypeLits as modulus, and Int64 as underyling
-- representation (needed to use with CT backend).  The modulus
-- doesn't have to be "good", but "good" moduli are faster.
type Zq q = ZqBasic q Int64 -- uses PolyKinds
type CTZq1 = Zq 536937857
type CTZq2 = (Zq 536972801, CTZq1)
type CTZq3 = (Zq 537054337, CTZq2)

type KSGad = TrivGad -- can also use (BaseBGad 2), for example

type CTRing1 t = CT PTIndex PTZq (Cyc t CTIndex CTZq1)
type CTRing2 t = CT PTIndex PTZq (Cyc t CTIndex CTZq2)
type SKRing t = Cyc t CTIndex (LiftOf PTZq)

-- | Simple example of how to use the SymmSHE application.
sheMain :: forall t . (_) => Proxy t -> IO ()
sheMain _ = do
  plaintext <- getRandom
  sk :: SK (SKRing t) <- genSK (1 :: Double)
  -- encrypt with a single modulus
  ciphertext :: CTRing1 t <- encrypt sk plaintext

  let ct1 = 2*ciphertext
      pt1 = decrypt sk ct1
  print $ "Test1 (expect TRUE): " ++ (show $ 2*plaintext == pt1)

  hint :: KSQuadCircHint KSGad (Cyc t CTIndex CTZq2) <- ksQuadCircHint sk
  let ct2 = modSwitch $ keySwitchQuadCirc hint $ modSwitch $ ciphertext*ciphertext
      pt2 = decrypt sk (ct2 :: CTRing1 t)
  -- note: this requires a *LARGE* CT modulus to succeed
  print $ "Test2 (expect FALSE): " ++ (show $ plaintext*plaintext == pt2)

  -- so we support using *several* small moduli:
  hint' :: KSQuadCircHint KSGad (Cyc t CTIndex CTZq3) <- ksQuadCircHint sk
  ciphertext' :: CTRing2 t <- encrypt sk plaintext
  let ct3 = keySwitchQuadCirc hint' $ modSwitch $ ciphertext' * ciphertext'
      pt3 = decrypt sk ct3
      ct3' = modSwitch ct3 :: CTRing1 t
      -- after rescaling, ct3' has a single modulus, so we can use normal decrypt
      pt3' = decrypt sk ct3'
  print $ "Test3 (expect TRUE): " ++ (show $ (plaintext*plaintext == pt3) && (pt3' == pt3))

