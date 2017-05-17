{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Arithmetic where

import Control.Monad.Reader
import Control.Monad.Writer

import Crypto.Alchemy.MonadAccumulator
--import Crypto.Alchemy.Interpreter.DedupRescale
import Crypto.Alchemy.Interpreter.Depth
import Crypto.Alchemy.Interpreter.Dup
import Crypto.Alchemy.Interpreter.ErrorRateWriter
import Crypto.Alchemy.Interpreter.Eval
import Crypto.Alchemy.Interpreter.KeysHints
import Crypto.Alchemy.Interpreter.Params
import Crypto.Alchemy.Interpreter.Print
import Crypto.Alchemy.Interpreter.PT2CT
import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Interpreter.Size

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

import Crypto.Lol                       hiding (Pos (..))
import Crypto.Lol.Applications.SymmSHE  hiding (CT,encrypt,decrypt)
import qualified Crypto.Lol.Applications.SymmSHE as SHE
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Types

import Control.Applicative
import Control.Monad.Random
import Data.Maybe
import Data.Type.Natural (Nat (Z))

-- EAC: We can get rid of signatures once #13524 is fixed (should be in 8.2)

-- we give a type signature for easy partial type application

addMul :: forall a e expr .
  (Add expr a, Lambda expr)
  => expr e (a -> a -> a)
addMul = lam $ lam $ v0 +: v0
{-
addMul :: forall b e expr a .
  (a ~ PreMul expr b, Mul expr b, Add expr a, Lambda expr)
  => expr e (a -> a -> b)
addMul = lam $ lam $ v0 *: (v0 +: v1)
-}
type Zq q = ZqBasic q Int64

argToReader :: (MonadReader v mon) => (v -> a -> mon b) -> a -> mon b
argToReader f a = flip f a =<< ask

main :: IO ()
main = do

  -- no types needed to show a function!
  putStrLn $ "PT expression: " ++ pprint addMul

  putStrLn $ "PT expression size: " ++ (show $ size addMul)
  putStrLn $ "Expression depth: "   ++ (show $ depth addMul)
  -- evaluate a DSL function to a Haskell function, then apply to arguments
  pt1 <- getRandom
  pt2 <- getRandom
  let ptresult = eval (addMul @(Cyc CT F4 (Zq 7))) pt1 pt2
  putStrLn $ "PT evaluation result: " ++ show ptresult

  let ptexpr = addMul @(PNoise 'Z (Cyc CT F4 (Zq 7)))
  putStrLn $ "PT expression params:\n" ++ (params ptexpr addMul)

  evalKeysHints (0.01 :: Double) $ do

    -- compile the un-applied function to CT, then print it out
    x <- argToReader (pt2ct
           @'[ '(F4, F512) ]
           -- @'[Zq $(mkTLNatNat 1312235009), Zq $(mkTLNatNat 37633) ] -- (still) fails with TrivGad
           -- @'[Zq $(mkTLNatNat 268440577), Zq $(mkTLNatNat 36097), Zq $(mkTLNatNat 36353), Zq $(mkTLNatNat 37633) ] --  (still) fails with TrivGad
           -- @'[Zq $(mkTLNatNat 268440577), Zq $(mkTLNatNat 65537)] succeeded with TrivGad, even before changes
           -- @'[Zq $(mkTLNatNat 36097), Zq $(mkTLNatNat 36353), Zq $(mkTLNatNat 37633) ] -- succeeds with TrivGad
           @'[Zq $(mkTLNatNat 1312235011), Zq $(mkTLNatNat 36101) ] -- bad moduli (30.3 bits) = huge error, even after addition
           -- @'[Zq $(mkTLNatNat 536870917), Zq $(mkTLNatNat 36101) ]  -- bad moduli = huge error, *only* after mul! (after addition, it's still 10^-5)
           -- @'[Zq $(mkTLNatNat 36101), Zq $(mkTLNatNat 36355), Zq $(mkTLNatNat 37635) ] -- bad modulus, but works fine?
           @TrivGad -- (BaseBGad 2)
           @Int64
           @Double)
           ptexpr

    -- duplicate the compiled expression
    let (z1,z2) = dup x
        (w1,w2) = dup z1
    -- encrypt some arguments
    arg1 <- argToReader encrypt pt1
    arg2 <- argToReader encrypt pt2
    -- print the compiled function
    liftIO $ putStrLn $ "CT expression: " ++ pprint w1
    liftIO $ putStrLn $ "CT expression params:\n" ++ params w1 w2
    --liftIO $ putStrLn $ "CT expression size: " ++ (show $ size w2)

    z2' <- readerToAccumulator $ writeErrorRates @Int64 @() z2
    let (result,errors) = runWriter $ eval z2' (return arg1) (return arg2)
    liftIO $ print $ "Error rates: " ++ show errors

    -- show the encrypted result
    --liftIO $ putStrLn $ "Encrypted evaluation result: " ++ show result
    -- show the decrypted result
    decResult <- fromJust <$> (readerToAccumulator $ decrypt result)
    liftIO $ putStrLn $ "Decrypted evaluation result: " ++ show decResult

    liftIO $ putStrLn $ if decResult == ptresult then "PASS" else "FAIL"


-- direct SHE computation
  pt :: PT (Cyc CT F4 (Zq 7))  <- getRandom
  sk :: SK (Cyc CT F512 Int64) <- genSK (0.01 :: Double)
  ct :: SHE.CT F4 (Zq 7) (Cyc CT F512 (Zq $(mkTLNatNat 1312235011))) <- SHE.encrypt sk pt
  print $ errorRate sk ct
  print $ errorRate sk (ct+ct)


errorRate :: forall t m' m z zp zq ct .
  (ErrorTermUCtx t m' z zp zq, Mod zq, ToInteger (LiftOf zq), ct ~ SHE.CT m zp (Cyc t m' zq))
  => SK (Cyc t m' z) -> ct -> Double
errorRate sk ct =
  (fromIntegral $ maximum $ fmap abs $ errorTermUnrestricted sk ct) / (fromIntegral $ proxy modulus (Proxy::Proxy zq))
