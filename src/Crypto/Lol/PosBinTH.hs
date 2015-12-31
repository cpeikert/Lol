{-# LANGUAGE TemplateHaskell #-}

-- | Defines Template Haskell functions for creating 'Pos' and 'Bin'
-- types.  The definitions of 'pos' and 'bin' need to be in a separate
-- module from where they are invoked, which is the only reason for
-- this extra module.

module Crypto.Lol.PosBinTH where

import Crypto.Lol.PosBinDefs

import Language.Haskell.TH

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
