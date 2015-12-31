{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, InstanceSigs,
             KindSignatures, PolyKinds, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies, UndecidableInstances #-}

-- | This sub-module exists only because we can't define and use
-- template Haskell splices in the same module.

module Crypto.Lol.PosBinDefs where

import Data.Singletons.Prelude
import Data.Singletons.TH
import Language.Haskell.TH

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

-- | Generate a Template Haskell splice for the type-level 'Pos'
-- representing a given 'Int', e.g., @$(pos 8)@.
pos :: Int -> TypeQ
pos n
    | n <= 0 = error "pos requires a positive argument"
    | n == 1 = conT 'O
    | otherwise = conT 'S `appT` pos (n-1)

-- | Generate a Template Haskell splice for the type-level 'Bin'
-- representing a given 'Int', e.g., @$(bin 89)@.
bin :: Int -> TypeQ
bin n
    | n <= 0 = error "bin requires a positive argument"
    | otherwise = case n `quotRem` 2 of
                    (0,1) -> conT 'B1
                    (q,0) -> conT 'D0 `appT` bin q
                    (q,1) -> conT 'D1 `appT` bin q
                    _ -> error "internal error in PosBinTH.bin"

-- | Generate a Template Haskell splice for a type synonym definition
-- @type <pfx>i = f i@.
conType :: String               -- ^ @pfx@
        -> (Int -> TypeQ)       -- ^ @f@
        -> Int                  -- ^ @i@
        -> DecQ
conType pfx f i = tySynD (mkName $ pfx ++ show i) [] (f i)

-- standard Sieve of Eratosthenes
-- defined here because we use it in a TH splice in PosBin
primes :: [Int]
primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
