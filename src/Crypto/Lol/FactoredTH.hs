module Crypto.Lol.FactoredTH where

import Crypto.Lol.PosBinTH

import Control.Arrow
import Data.List
import Language.Haskell.TH

fTypePrime :: Int -> DecQ
fTypePrime i = do
  let name = mkName $ 'F' : show i
  typ <- conT $ mkName $ "Prime" ++ show i
  return $ TySynD name [] typ

-- | First argument is infiniten list of primes left to consider.
factorize' :: [Int] -> Int -> [Int]
factorize' _ 1 = []
factorize' ds@(d:ds') n =
  if d * d > n then [n]
  else let (q,r) = n `divMod` d
       in if r == 0 then d : factorize' ds q
          else factorize' ds' n

factorize :: Int -> [(Int,Int)]
factorize = map (head &&& length) . group . factorize' primes
