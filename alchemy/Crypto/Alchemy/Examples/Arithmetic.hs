{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Crypto.Alchemy.Examples.Arithmetic where

import Control.Monad.Reader

import Crypto.Alchemy.Interpreter.Dup
import Crypto.Alchemy.Interpreter.Eval
import Crypto.Alchemy.Interpreter.Print
import Crypto.Alchemy.Interpreter.Compiler.PNoise
import Crypto.Alchemy.Interpreter.Compiler.PT2CT

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

import Crypto.Lol hiding (Pos(..))
import qualified Crypto.Lol.Applications.SymmSHE as SHE
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Types

import Data.Maybe
import Data.Type.Natural (Nat (Z))

-- EAC: We can get rid of signatures once #13524 is fixed (should be in 8.2)

-- we give a type signature for easy partial type application
pt1 :: forall b e expr a .
  (a ~ PreMul expr b, Mul expr b, Add expr a, DB expr a, Lambda expr)
  => expr e (a -> a -> b)
pt1 = lam $ lam $ v0 *: (v0 +: (s v0))

type Zq q = ZqBasic q Int64

main :: IO ()
main = do
  -- look Ma, no types!
  putStrLn $ pprint pt1
  putStrLn $ show $ eval (pt1 @Int) 7 11

  -- compile the un-applied function to CT, then print it out
  (x,st) <- compile
         @'[ '(F4, F8) ]
         @'[ Zq $(mkQ $ 2^(40 :: Int))] -- , Zq $(mkQ 11) ]
         @(Zq $(mkQ 13))
         @TrivGad
         @Double
         1.0
         (pt1 @(PNoise 'Z (Cyc CT F4 (Zq 7))))

  let (z1,z2) = dup x
  arg1 <- fromJust $ encryptArg st 7
  arg2 <- fromJust $ encryptArg st 11
  putStrLn $ pprint z1
  putStrLn $ show $ eval z2 arg1 arg2

type B = SHE.CT F4 (Zq 7) (Cyc CT F8 (Zq $(mkQ $ 2^(40 :: Int))))
type A = B
type F = (A,A) -> (A,A) -> (B,B)
{-
foo ::
  (m'map ~ '[ '(F4, F8) ],
   zqs ~ '[ Zq $(mkQ $ 2^(4 :: Int))],
   ksmod ~ Zq $(mkQ 13),
   mon ~ ReaderT Double (StateT P2CState IO),
   expr ~ PT2CT m'map zqs ksmod TrivGad Double P mon,
   b ~ PNoise 'Z (Cyc CT F4 (Zq 7)),
   a ~ PreMul expr b)
  => expr () (a -> a -> b)
foo = pt1
-}

type Zqs = '[ Zq $(mkQ $ 2^(4 :: Int)) ] -- , Zq $(mkQ 11) ]

-- write an interpreter to remove "rescale a -> a"
-- warnings about moduli mismatches
-- encapsulation for compile CTs? (CTWrapper?)