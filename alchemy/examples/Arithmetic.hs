{-# LANGUAGE DataKinds             #-}
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
import Crypto.Alchemy.Interpreter.PT2CT
import Crypto.Alchemy.Interpreter.KeysHints
import Crypto.Alchemy.Interpreter.PT2CT.Noise
--import Crypto.Alchemy.Interpreter.DedupRescale
import Crypto.Alchemy.Interpreter.Dup
import Crypto.Alchemy.Interpreter.ErrorRateWriter
import Crypto.Alchemy.Interpreter.Eval
import Crypto.Alchemy.Interpreter.Print

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

import Crypto.Lol                       hiding (Pos (..))
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Types

import Data.Maybe
import Data.Type.Natural (Nat (Z))

-- EAC: We can get rid of signatures once #13524 is fixed (should be in 8.2)

-- we give a type signature for easy partial type application
pt1 :: forall b e expr a .
  (a ~ PreMul expr b, Mul expr b, Add expr a, Lambda expr)
  => expr e (a -> a -> b)
pt1 = lam $ lam $ v0 *: (v0 +: v1)

type Zq q = ZqBasic q Int64

argToReader :: (MonadReader v mon) => (v -> a -> mon b) -> a -> mon b
argToReader f a = flip f a =<< ask

main :: IO ()
main = do
  -- no types needed to show a function!
  putStrLn $ pprint pt1
  -- evaluate a DSL function to a Haskell function, then apply to arguments
  putStrLn $ show $ eval (pt1 @Int) 7 11

  evalKeysHints (0.01 :: Double) $ do

    -- compile the un-applied function to CT, then print it out
    x <- argToReader (pt2ct
           @'[ '(F4, F512) ]
           @'[ Zq $(mkTLNatNat 36097), Zq $(mkTLNatNat 36353), Zq $(mkTLNatNat 37633) ]
           @TrivGad
           @Int64
           @Double)
           (pt1 @(PNoise 'Z (Cyc CT F4 (Zq 7))))

    -- duplicate the compiled expression
    let (z1,z2) = dup x
        (w1,w2) = dup z1
    -- encrypt some arguments
    arg1 <- argToReader encrypt 7
    arg2 <- argToReader encrypt 11
    -- print the compiled function
    liftIO $ putStrLn $ pprint w1
    -- if the first modulus is large enough, this will remove the rescales!
    liftIO $ putStrLn $ pprint $ w2 -- dedupRescale w2
    -- evaluate the compiled function, then apply it to encrypted inputs
    --let result = eval z2 arg1 arg2

    z2' <- readerToAccumulator $ writeErrorRates @_ @Int64 z2
    let (result,errors) = runWriter $ eval z2' (return arg1) (return arg2)
    liftIO $ print errors

    -- show the encrypted result
    liftIO $ putStrLn $ show result
    -- show the decrypted result
    decResult <- readerToAccumulator $ decrypt result
    liftIO $ putStrLn $ show $ advisePow $ fromJust decResult

-- EAC: TODO
-- encapsulation for compile CTs? (CTWrapper?)
-- tunneling example
