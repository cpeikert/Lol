{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, PolyKinds,
             RebindableSyntax, ScopedTypeVariables, TypeFamilies,
             UndecidableInstances #-}

-- | Orphan instance of 'IrreduciblePoly' for characteristic-2 fields.

module Crypto.Lol.Types.IrreducibleChar2 () where

import Crypto.Lol.Prelude hiding (lookup)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.FiniteField hiding (fromList)

import Data.Map hiding (map)
import Data.Maybe (fromMaybe)

instance (CharOf a ~ Prime2, Field a) => IrreduciblePoly a where
  irreduciblePoly = do
    n <- value
    return $ flip fromMaybe (lookup n polyMap) $
      error $
        "The IrreduciblePoly instance for N2 included with the library " ++
        "(and exported by Crypto.Lol) only contains irreducible polynomials " ++
        "for characteristic-2 fields up to GF(2^^32). You need a polynomial " ++
        "for GF(2^^" ++ (show n) ++ "). Define your own instance of " ++
        "IrreduciblePoly and do not import Crypto.Lol."

polyMap :: (Ring fp) => Map Int (Polynomial fp)
polyMap = fromList $ map (\xs -> (head xs, coeffsToPoly xs)) coeffs

coeffsToPoly :: (Ring fp) => [Int] -> Polynomial fp
coeffsToPoly [a,b] = X^^a + X^^b + 1
coeffsToPoly [a,b,c,d] = X^^a + X^^b + X^^c + X^^d + 1

-- The list below is small portion of the table
-- "Table of Low-Weight binary Irreducible Polynomials" at
-- http://www.hpl.hp.com/techreports/98/HPL-98-135.pdf?jumpid=reg_R1002_USEN
-- The lists are coefficients of minimal polynomials:
--   [a,b] is the irreducible trinomial X^a + X^b + 1
--   [a,b,c,d] is the irreducible pentanomial X^a + X^b + X^c + X^d + 1
coeffs :: [[Int]]
coeffs = [
  [1,1],
  [2,1],
  [3,1],
  [4,1],
  [5,2],
  [6,1],
  [7,1],
  [8,4,3,1],
  [9,1],
  [10,3],
  [11,2],
  [12,3],
  [13,4,3,1],
  [14,5],
  [15,1],
  [16,5,3,1],
  [17,3],
  [18,3],
  [19,5,2,1],
  [20,3],
  [21,2],
  [22,1],
  [23,5],
  [24,4,3,1],
  [25,3],
  [26,4,3,1],
  [27,5,2,1],
  [28,1],
  [29,2],
  [30,1],
  [31,3],
  [32,7,3,2],
  [33,10],
  [34,7],
  [35,2],
  [36,9],
  [37,6,4,1],
  [38,6,5,1],
  [39,4],
  [40,5,4,3],
  [41,3],
  [42,7],
  [43,6,4,3],
  [44,5],
  [45,4,3,1],
  [46,1],
  [47,5],
  [48,5,3,2],
  [49,9],
  [50,4,3,2],
  [51,6,3,1],
  [52,3],
  [53,6,2,1],
  [54,9],
  [55,7],
  [56,7,4,2],
  [57,4],
  [58,19],
  [59,7,4,2],
  [60,1],
  [61,5,2,1],
  [62,29],
  [63,1],
  [64,4,3,1],
  [65,18],
  [66,3],
  [67,5,2,1],
  [68,9],
  [69,6,5,2],
  [70,5,3,1],
  [71,6],
  [72,10,9,3],
  [73,25],
  [74,35],
  [75,6,3,1],
  [76,21],
  [77,6,5,2],
  [78,6,5,3],
  [79,9],
  [80,9,4,2],
  [81,4],
  [82,8,3,1],
  [83,7,4,2],
  [84,5],
  [85,8,2,1],
  [86,21],
  [87,13],
  [88,7,6,2],
  [89,38],
  [90,27],
  [91,8,5,1],
  [92,21],
  [93,2],
  [94,21],
  [95,11],
  [96,10,9,6],
  [97,6],
  [98,11],
  [99,6,3,1],
  [100,15],
  [101,7,6,1],
  [102,29],
  [103,9],
  [104,4,3,1],
  [105,4],
  [106,15],
  [107,9,7,4],
  [108,17],
  [109,5,4,2],
  [110,33],
  [111,10],
  [112,5,4,3],
  [113,9],
  [114,5,3,2],
  [115,8,7,5],
  [116,4,2,1],
  [117,5,2,1],
  [118,33],
  [119,8],
  [120,4,3,1],
  [121,18],
  [122,6,2,1],
  [123,2],
  [124,19],
  [125,7,6,5],
  [126,21],
  [127,1],
  [128,7,2,1]
  ]