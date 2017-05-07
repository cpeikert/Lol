{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators  #-}


module Crypto.Alchemy.Examples.Tunnel where


--{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
--{-# OPTIONS_GHC -fno-warn-missing-signatures      #-}

--import Control.Applicative
--import Control.Monad.Identity
import Crypto.Alchemy.Interpreter.Dup
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.Tunnel


import Crypto.Alchemy.Interpreter.Eval
--import Crypto.Alchemy.Interpreter.DedupRescale
import Crypto.Alchemy.Interpreter.Print
import Crypto.Alchemy.Interpreter.PT2CT

import Crypto.Lol hiding (Pos(..))
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Types
import Crypto.Lol.Cyclotomic.Tensor (TElt) -- EAC: I shouldn't need to explicitly import this
import Crypto.Lol.Types.ZPP -- EAC: I shouldn't need to explicitly import this...

--import Data.Functor.Trans.Tagged
import Data.Type.Natural

-- EAC: We can get rid of signatures once #13524 is fixed (should be in 8.2)

tunn1 :: (Tunnel expr eru r u, Tunnel expr eus u s, Lambda expr)
  => LinearOf expr eru r u -> LinearOf expr eus u s -> expr env (r -> s)
tunn1 f1 f2 = lam $ tunnel f2 $: (tunnel f1 $: v0)

-- EAC: redundant pattern match is a bug: https://ghc.haskell.org/trac/ghc/ticket/13651
monoTunn1 :: forall r u cs expr eru eus t zp cr cu h s env .
  (Tunnel expr eru cr cu, Tunnel expr eus cu cs, Lambda expr,
   LinearOf expr eru cr cu ~ Tagged h (Linear t zp eru r u),
   LinearOf expr eus cu cs ~ Tagged ('S h) (Linear t zp eus u s))
  => Tagged h (Linear t zp eru r u) -> Tagged ('S h) (Linear t zp eus u s) -> expr env (cr -> cs)
monoTunn1 = tunn1

type Zq q = ZqBasic q Int64

main :: IO ()
main = do
  let (exp1a, exp1b) = dup $ monoTunn1
        @H0
        @H1
        @(PNoise 'Z (Cyc CT H2 (Zq PP8)))
        (tag decToCRT)
        (tag decToCRT)
  -- example with rescale de-duplication when tunneling
  -- print the unapplied PT function
  putStrLn $ pprint exp1a
  putStrLn $ pprint exp1b
   {-
  -- compile the up-applied function to CT, then print it out
  (y,_) <- compile
         @'[ '(H0, H0'), '(H1,H1'), '(H2, H2') ]
         @'[ Zq 7, (Zq 11, Zq 7) ]
         @'[ '(Zq 7, (Zq 11, Zq 7)), '((Zq 11, Zq 7), (Zq 13, (Zq 11, Zq 7))) ]
         @TrivGad
         @Int64
         @Double
         exp1b
  -- compile once, interpret with multiple ctexprs!!
  let (z1,z2) = dupCT y
  putStrLn $ pprint z1
  putStrLn $ pprint $ dedupRescale z2
-}
type H0 = F8
type H1 = F4 * F7
type H2 = F2 * F7 * F13
type H0' = H0 * F7 * F13
type H1' = H1 * F13
type H2' = H2
{-
-- EAC: This is copied from HomomPRF, but it needs a permanent home.
type TunnelPTCtx' expr d t e r s zp =
  (e ~ FGCD r s,                                     -- type restriction for simplicity
   --Tunnel expr e (cr) cs, TunnelCtxPT expr d t e r s zp, -- call to tunnelPT
   e `Divides` r, e `Divides` s, CElt t zp,          -- linearDec
   ZPP zp, TElt t (ZpOf zp))                         -- crtSet
-}

type FunCtx t r s zp = FunCtx' t (FGCD r s) r s zp

type FunCtx' t e r s zp =
  (e `Divides` r, e `Divides` s, CElt t zp,  -- linearDec
   ZPP zp, TElt t (ZpOf zp))

-- linear function mapping decoding basis coefficients to CRT slots
decToCRT :: forall s t r zp e . (FunCtx t r s zp, e ~ FGCD r s) => Linear t zp e r s
decToCRT =
  let crts = proxy crtSet (Proxy::Proxy e)
      r = proxy totientFact (Proxy::Proxy r)
      e = proxy totientFact (Proxy::Proxy e)
      dim = r `div` e
      -- only take as many crts as we need
      -- otherwise linearDec fails
  in linearDec $ take dim crts
