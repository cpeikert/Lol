module Crypto.Lol.FactoredTH where

import Crypto.Lol.PosBinTH
import Crypto.Lol.FactoredDefs

import Control.Arrow
import Data.List
import Language.Haskell.TH

-- | Factorize a positive integer into an ordered list of its prime
-- divisors, with multiplicities.  First argument is infinite list of
-- primes left to consider.
factorize' :: [Int] -> Int -> [Int]
factorize' _ 1 = []
factorize' ds@(d:ds') n =
  if d * d > n then [n]
  else let (q,r) = n `divMod` d
       in if r == 0 then d : factorize' ds q
          else factorize' ds' n

-- | Factorize a positive integer into a list of (prime,exponent)
-- pairs, in strictly increasing order by prime.
factorize :: Int -> [(Int,Int)]
factorize = map (head &&& length) . group . factorize' primes

-- | Generate a Template Haskell splice for a type synonym definition
-- of a factored type @Fn@ for a positive integer @n@.
fType :: Int -> DecQ
fType n = tySynD (mkName $ 'F' : show n) [] (promotePPs $ factorize n)
  where promotePPs :: [(Int,Int)] -> TypeQ
        promotePPs = foldr (\pp -> appT (promotedConsT `appT` promotePP pp)) promotedNilT

        promotePP :: (Int,Int) -> TypeQ
        promotePP (p,e) = promotedTupleT 2 `appT` bin p `appT` pos e
