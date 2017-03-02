{-|
Module      : Main
Description : Example using the unmaintained compiler.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Homomorphic ring tunneling and rounding, aided by the unmaintained compiler.
-}

{-# LANGUAGE RebindableSyntax, DataKinds, TypeOperators,
             KindSignatures, TypeFamilies, UndecidableInstances, PartialTypeSignatures,
             FlexibleInstances, MultiParamTypeClasses, PackageImports, GADTs,
             FlexibleContexts, ScopedTypeVariables, RankNTypes, PolyKinds,
             StandaloneDeriving, ConstraintKinds, TypeSynonymInstances #-}

import Crypto.Lol.Compiler.AST hiding (Sub)
import Crypto.Lol.Compiler.CT
import Crypto.Lol.Compiler.CTDummy
import Crypto.Lol.Compiler.CTCompiler
import Language.Syntactic.Sugar.BindingTyped () -- need for instance of Syntactic (a->b) when using `share` below
import Language.Syntactic hiding (size)
import Language.Syntactic.Functional (lamTyped, BindingT)

import Crypto.Lol as Lol hiding ((**))
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Applications.SymmSHE hiding (CT)
import qualified Crypto.Lol.Applications.SymmSHE as SHE
import Crypto.Lol.CRTrans
import Crypto.Lol.Types.Random

import Types

import Data.Promotion.Prelude.List


import Control.Monad.Random
import "crypto-api" Crypto.Random
import Crypto.Random.DRBG

import Data.Foldable
import Data.Map as M hiding (replicate, size, map, toList, (\\))

-- don't turn on -fbreak-on-exception unless you want to trace because it makes the error messages much worse!

type CTDOM = Typed (ADDITIVE :+: RING :+: CTOps :+: CTDummyOps :+: Literal :+: Let :+: BindingT)
type CTExpr a = ASTF CTDOM a

main :: IO ()
main = do
  print "CT:"

  tunnelTest (Proxy::Proxy CT)
  prfTest (Proxy::Proxy CT)

  print "RT:"
  tunnelTest (Proxy::Proxy RT)
  prfTest (Proxy::Proxy RT)

  print "Done"

-- This function serves the dual purpose of specifying the random generator
-- and keeping all of the code in the IO monad, which helps write clean code below
-- No sequencing occurs between separate calls to this function, but it would be hard
-- to get timing any other way.
evalHashDRBG :: Rand (CryptoRand HashDRBG) a -> IO a
evalHashDRBG = evalCryptoRandIO Proxy

tunnelTest pc@(_::Proxy t) = do -- in IO
    let v = 0.1 :: Double
    x'<- getRandom

    ast0 <- time "Computing AST: " $ lamTyped $ tunnelAST pc

    (ast1, idMap) <- time "Generating keys: " =<<
      evalHashDRBG (genKeys v ast0)

    let keyMap = M.fromList $ elems idMap

    ast2 <- time "Generating hints: " =<<
      evalHashDRBG (genHints keyMap ast1)

    (x1,encsk) <- time "Encrypting input: " =<<
      evalHashDRBG (encryptInput idMap x')

    putStrLn $ "Input noise level:\t" ++ (show $ errRatio x1 encsk)

    let decsk = getDecryptKey keyMap ast2

    ans <- time "Evaluating AST: " $ eval ast2 x1

    putStrLn $ "Output noise level:\t" ++ (show $ errRatio ans decsk)

    putStrLn "Done!"

errRatio :: forall m zp t m' zq z .
  (Reduce z zq, Lift' zq, CElt t z, ToSDCtx t m' zp zq, Mod zq, Absolute (LiftOf zq),
   Ord (LiftOf zq), ToInteger (LiftOf zq))
  => SHE.CT m zp (Cyc t m' zq) -> SK (Cyc t m' z) -> Double
errRatio ct sk =
  (fromIntegral $ maximum $ fmap abs $ errorTermUnrestricted sk ct) /
  (fromIntegral $ proxy modulus (Proxy::Proxy zq))

tunnelAST (_::Proxy t) (x :: CTExpr (SHE.CT H0 ZP2 (Cyc t H0' ZQ5))) =
  roundCTDown $
  --tunnHelper (Proxy::Proxy '(H5,H5',ZQ6)) $
  --tunnHelper (Proxy::Proxy '(H4,H4',ZQ6)) $
  --tunnHelper (Proxy::Proxy '(H3,H3',ZQ6)) $
  --tunnHelper (Proxy::Proxy '(H2,H2',ZQ6)) $
  tunnHelper2 $
  roundCTUp x

prfTest pc@(_::Proxy t) = do
  -- public multiplier
  c <- getRandom
  let v = 0.1 :: Double
  x' <- getRandom

  ast0 <- time "Computing AST: " $ lamTyped $ homomPRF c

  (ast1, idMap) <- time "Generating keys: " =<<
    evalHashDRBG (genKeys v ast0)

  ast2 <- time "Generating hints: " =<<
    evalHashDRBG (genHints (M.fromList $ elems idMap) ast1)

  (x :: SHE.CT H0 ZP8 (Cyc t H0' ZQ4),_) <- time "Encrypting input: " =<<
    evalHashDRBG (encryptInput idMap x')

  _ <- time "Evaluating AST: " $ eval ast2 x

  putStrLn "Done!"

homomPRF c x =
  let scaledX = mulPublicCT c x
      {-hopX = roundCTDown $
               roundCTDown $
               tunnHelper2 $
               tunnHelper2 $
               tunnHelper2 $
               tunnHelper2 $
               tunnHelper2 $
               roundCTUp scaledX -} -- scale up modulus for applying hints
      hopX = tunnel (Proxy::Proxy RngList) scaledX
  in share hopX $ \y ->
    let z = y ** (addPublicCT one y)          -- x*(x+1)
        w = roundPTDown z                     -- round PT to ZP4
    in share w $ \w' ->                       -- w/w' has PT ZP4 and CT ZQ2
      let u = w' ** (addPublicCT (-one) w')   -- u = w*(w-1)
      in roundPTDown u                        -- round the PT to ZP2 and return

-- type restricted helpers

(**) :: (Ring (CTExpr (SHE.CT m zp (Cyc c m' zq))),
         KSDummyCtx z TrivGad (ZQUp zq) m zp c m' zq,
         ASTRoundCTCtx CTDOM dom' c m m' zp zq (ZQDown zq))
  => CTExpr (SHE.CT m zp (Cyc c m' zq))
     -> CTExpr (SHE.CT m zp (Cyc c m' zq))
     -> CTExpr (SHE.CT m zp (Cyc c m' (ZQDown zq)))
a ** b =
  let c = a*b
      c' = ksqHelper c
  in roundCTDown c'

ksqHelper :: forall m zp c m' zq z . (KSDummyCtx z TrivGad (ZQUp zq) m zp c m' zq)
  => CTExpr (SHE.CT m zp (Cyc c m' zq)) -> CTExpr (SHE.CT m zp (Cyc c m' zq))
ksqHelper u = proxy (ksqDummy u) (Proxy::Proxy '(TrivGad, ZQUp zq))

tunnHelper2 :: (ASTTunnelCtx dom dom' gad c r r' s s' e e' z zp zq,
   e' ~ (e * (r' / r)), -- see #11248 and 11249
   gad ~ TrivGad, dom ~ CTDOM, '(s,s') ~ NextListElt '(r,r') RngList)
  => CTExpr (SHE.CT r zp (Cyc c r' zq)) -> CTExpr (SHE.CT s zp (Cyc c s' zq))
tunnHelper2 x = proxy (tunnDummy x) (Proxy::Proxy TrivGad)

roundCTUp :: (ASTRoundCTCtx CTDOM dom' c m m' zp zq zq', zq' ~ ZQUp zq)
  => CTExpr (SHE.CT m zp (Cyc c m' zq)) -> CTExpr (SHE.CT m zp (Cyc c m' zq'))
roundCTUp x = roundCT x

roundCTDown :: (ASTRoundCTCtx CTDOM dom' c m m' zp zq zq', zq' ~ ZQDown zq)
  => CTExpr (SHE.CT m zp (Cyc c m' zq)) -> CTExpr (SHE.CT m zp (Cyc c m' zq'))
roundCTDown x = roundCT x

roundPTDown :: (ASTRoundPTCtx CTDOM dom' c m m' zp zp' zq, zp' ~ ZPDiv2 zp)
  => CTExpr (SHE.CT m zp (Cyc c m' zq)) -> CTExpr (SHE.CT m zp' (Cyc c m' zq))
roundPTDown x = roundPT x

class Tunnel xs c zp zq where
  tunnel' :: (Head xs ~ '(r,r'), Last xs ~ '(s,s')) =>
    Proxy xs -> CTExpr (SHE.CT r zp (Cyc c r' zq)) -> CTExpr (SHE.CT s zp (Cyc c s' zq))

instance Tunnel '[ '(m,m') ] c zp zq where
  tunnel' _ = id

instance (ASTTunnelCtx CTDOM dom' TrivGad c r r' t t' e e' z zp zq,
          Tunnel ('(t,t') ': rngs) c zp zq)
  => Tunnel ('(r,r') ': '(t,t') ': rngs) c zp zq where
  tunnel' _ = tunnel' (Proxy::Proxy ('(t,t') ': rngs)) . (\x -> proxy (tunnDummy x) (Proxy::Proxy TrivGad))

tunnel :: (Head rngs ~ '(r,r'), Last rngs ~ '(s,s'), Tunnel rngs c zp (ZQUp zq), ZQDown (ZQUp zq) ~ zq,
           ASTRoundCTCtx CTDOM dom' c r r' zp zq (ZQUp zq),
           ASTRoundCTCtx CTDOM dom' c s s' zp (ZQUp zq) zq,
           ASTRoundCTCtx CTDOM dom' c s s' zp zq (ZQDown zq)) =>
  Proxy rngs -> CTExpr (SHE.CT r zp (Cyc c r' zq)) -> CTExpr (SHE.CT s zp (Cyc c s' (ZQDown zq)))
tunnel rngs x = roundCTDown $ roundCTDown $ tunnel' rngs $ roundCTUp x
