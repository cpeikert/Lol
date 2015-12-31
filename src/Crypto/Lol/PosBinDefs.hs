{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, InstanceSigs,
             KindSignatures, PolyKinds, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Crypto.Lol.PosBinDefs where

import Data.Singletons.Prelude
import Data.Singletons.TH

singletons [d|
            -- | Positive naturals (1, 2, ...) in Peano representation.
            data Pos = O     -- ^ one
                     | S Pos -- ^ successor
                       deriving (Show, Eq)

            instance Ord Pos where
              compare O O          = EQ
              compare O (S _)      = LT
              compare (S _) O      = GT
              compare (S a) (S b)  = compare a b

            addPos :: Pos -> Pos -> Pos
            addPos O b      = S b
            addPos (S a) b  = S $ addPos a b

            subPos :: Pos -> Pos -> Pos
            subPos (S a) O      = a
            subPos (S a) (S b)  = subPos a b
            subPos O _          = error "Invalid call to subPos: a <= b"

           |]

posToInt :: Num z => Pos -> z
posToInt O = 1
posToInt (S a) = 1 + posToInt a

singletons [d|
            -- | Positive naturals in binary representation.
            data Bin = B1       -- ^ 1
                     | D0 Bin   -- ^ 2*b (double)
                     | D1 Bin   -- ^ 1 + 2*b (double and increment)
                       deriving (Show, Eq)

            instance Ord Bin where
              compare B1 B1          = EQ
              compare B1 (D0 _)      = LT
              compare B1 (D1 _)      = LT
              compare (D0 _) B1      = GT
              compare (D1 _) B1      = GT
              compare (D0 a) (D0 b)  = compare a b
              compare (D1 a) (D1 b)  = compare a b
              compare (D0 a) (D1 b)  = case compare a b of
                                       EQ -> LT
                                       LT -> LT
                                       GT -> GT
              compare (D1 a) (D0 b)  = case compare a b of
                                       EQ -> GT
                                       LT -> LT
                                       GT -> GT

           |]

-- not promotable due to numeric output

binToInt :: Num z => Bin -> z
binToInt B1 = 1
binToInt (D0 a) = 2 * binToInt a
binToInt (D1 a) = 1 + 2 * binToInt a

-- | Kind-restricted synonym for 'SingI'.
type PosC (p :: Pos) = SingI p

-- | Kind-restricted synonym for 'SingI'.
type BinC (b :: Bin) = SingI b

