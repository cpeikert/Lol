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
            -- Positive naturals (1, 2, ...) in Peano representation.
            data Pos = O     -- one
                     | S Pos -- successor
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
            -- Positive naturals in binary representation.
            data Bin = B1       -- 1
                     | D0 Bin   -- 2*b (double)
                     | D1 Bin   -- 1 + 2*b (double and increment)
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

-- | Template Haskell splice for the 'Pos' type
-- representing a given 'Int', e.g., @$(posType 8)@.
posType :: Int -> TypeQ
posType n
    | n <= 0 = fail $ "posType: non-positive argument n = " ++ show n
    | n == 1 = conT 'O
    | otherwise = conT 'S `appT` posType (n-1)

-- | Template Haskell splice for the 'Bin' type
-- representing a given 'Int', e.g., @$(binType 89)@.
binType :: Int -> TypeQ
binType n
    | n <= 0 = fail $ "binType: non-positive argument n = " ++ show n
    | otherwise = case n `quotRem` 2 of
                    (0,1) -> conT 'B1
                    (q,0) -> conT 'D0 `appT` binType q
                    (q,1) -> conT 'D1 `appT` binType q
                    _ -> fail "internal error in PosBinTH.bin"

posDec, binDec :: Int -> DecQ
-- | Template Haskell splice that defines the 'Pos' type synonym @Pn@.
posDec = intDec "P" posType
-- | Template Haskell splice that defines the 'Bin' type synonym @Bn@.
binDec = intDec "B" binType

-- | Template Haskell splice that declares a type synonym
-- @<pfx>n@ as the type @f i@.
intDec :: String               -- ^ @pfx@
       -> (Int -> TypeQ)       -- ^ @f@
       -> Int                  -- ^ @n@
       -> DecQ
intDec pfx f n = tySynD (mkName $ pfx ++ show n) [] (f n)

-- | Infinite list of primes, built using Sieve of Erastothenes.
primes :: [Int]
primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

-- | Search for the argument in 'primes'.  This is not particularly
-- fast, but works well enough for moderate-sized numbers that would
-- appear as (divisors of) cyclotomic indices of interest.
prime :: Int -> Bool
prime = go primes
    where go (p:ps) n = case compare p n of
                          LT -> go ps n
                          EQ -> True
                          GT -> False
