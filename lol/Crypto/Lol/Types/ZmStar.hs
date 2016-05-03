{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
             RebindableSyntax, ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

-- | A collection of helper functions for working with @Z_m^*@

module Crypto.Lol.Types.ZmStar
( order, partitionCosets
) where

import Crypto.Lol.Factored
import Crypto.Lol.Prelude       as LP hiding (null)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.ZqBasic

import Data.List as L (foldl', transpose)
import Data.Map  (Map, elems, empty, insertWith')
import Data.Set  as S (Set, difference, findMin, fromList, map, null)


-- | The multiplicative order of @p@ (the argument) modulo @m@.
-- Requires @gcd(p,m)=1@.
order :: forall m . (Reflects m Int) => Int -> Tagged m Int
order p = tag $
  let mval = proxy value (Proxy::Proxy m)
  in if gcd p mval /= 1
     then error "p and m not coprime"
     else 1 + length (takeWhile (/= one) $
                      tail $ iterate (* fromIntegral p) (one :: ZqBasic m Int))

-- given p, returns the cosets of Z_m^* / <p>
cosets :: forall zm . (Mod zm, ModRep zm ~ Int, Ord zm, Ring zm)
  => Int -> [Set zm]
cosets p =
  let mval = proxy modulus (Proxy::Proxy zm)
  in if gcd p mval /= 1
     then error "p and m not coprime"
     else let zmstar = fromList $ LP.map fromIntegral $ filter ((==) 1 . gcd mval) [1..mval]
              zp = fromIntegral p
              -- generates the coset containing x
              coset x = fromList $ x : takeWhile (/=x) (iterate (*zp) $ zp*x)
              -- repeatedly removes a (new) coset from the remaining elements
              genCosets s | null s = []
              genCosets s = let c = coset (findMin s)
                            in c : genCosets (difference s c)
          in genCosets zmstar

-- CJP: could tag this by '(p,m,m') for safety/memoization.

-- | Given @p@, returns a partition of the cosets of @Z_{m\'}^* \/ \<p>@
-- (specified by representatives), where the cosets in each component
-- are in bijective correspondence with the cosets of @Z_m^* \/ \<p>@ under
-- the natural (@mod m@) homomorphism.
partitionCosets :: forall m m' . (m `Divides` m')
  => Int -> Tagged '(m, m') [[Int]]
partitionCosets p =
  let m'cosets = cosets p
      -- a map from cosets of Z_m^* / <p> to their preimages under the
      -- natural homomorphism
      partition =
        L.foldl' (\cmap x -> insertWith' (++) (S.map (reduce . lift) x) [x] cmap)
          (empty :: Map (Set (ZqBasic m Int)) [Set (ZqBasic m' Int)])
          m'cosets
      -- transpose the map to get a list of list of sets, where for each
      -- inner list, there is exactly one m'-(co)set lying above each m-coset
      part' = transpose $ elems partition
     -- concat the inner sets to get a list of "CRT cosets" (indexed in Z_m'^*)
  in return $ LP.map (LP.map (lift . findMin)) part'
