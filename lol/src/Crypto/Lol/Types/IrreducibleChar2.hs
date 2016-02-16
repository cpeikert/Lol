{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, PolyKinds,
             RebindableSyntax, ScopedTypeVariables, TypeFamilies,
             UndecidableInstances #-}

-- | Orphan instance of 'IrreduciblePoly' for characteristic-2 fields.

module Crypto.Lol.Types.IrreducibleChar2 () where

import Crypto.Lol.LatticePrelude
import Crypto.Lol.Reflects
import Crypto.Lol.Types.FiniteField

-- | Convenience function.
taggedProxy :: Tagged s (Proxy s)
taggedProxy = tag Proxy

-- CJP: we should perhaps prefer ternary (or 5-ary) polynomials over
-- Conway polynomials, because we don't use the special properties of
-- Conways, and reduction modulo sparse polynomials should be faster.

-- conway
--generate in Python (or choose any irreducible polynomial)
-- to generate with Sage, start sage and type:
--      conway_polynomial(p,e)
-- then copy and paste
instance (CharOf a ~ Prime2, Field a) => IrreduciblePoly a where
  irreduciblePoly = do
    pn <- taggedProxy
    let n = proxy value pn :: Int
    return $ case n of
      1 -> X^^1 + 1
      2 -> X^^2 + X^^1 + 1
      3 -> X^^3 + X^^1 + 1
      4 -> X^^4 + X^^1 + 1
      5 -> X^^5 + X^^2 + 1
      6 -> X^^6 + X^^4 + X^^3 + X^^1 + 1
      7 -> X^^7 + X^^1 + 1
      8 -> X^^8 + X^^4 + X^^3 + X^^2 + 1
      9 -> X^^9 + X^^4 + 1
      10 -> X^^10 + X^^6 + X^^5 + X^^3 + X^^2 + X^^1 + 1
      11 -> X^^11 + X^^2 + 1
      12 -> X^^12 + X^^7 + X^^6 + X^^5 + X^^3 + X^^1 + 1
      13 -> X^^13 + X^^4 + X^^3 + X^^1 + 1
      14 -> X^^14 + X^^7 + X^^5 + X^^3 + 1
      15 -> X^^15 + X^^5 + X^^4 + X^^2 + 1
      16 -> X^^16 + X^^5 + X^^3 + X^^2 + 1
      17 -> X^^17 + X^^3 + 1
      18 -> X^^18 + X^^12 + X^^10 + X^^1 + 1
      19 -> X^^19 + X^^5 + X^^2 + X^^1 + 1
      20 -> X^^20 + X^^10 + X^^9 + X^^7 + X^^6 + X^^5 + X^^4 + X^^1 + 1
      21 -> X^^21 + X^^6 + X^^5 + X^^2 + 1
      22 -> X^^22 + X^^12 + X^^11 + X^^10 + X^^9 + X^^8 + X^^6 + X^^5 + 1
      23 -> X^^23 + X^^5 + 1
      24 -> X^^24 + X^^16 + X^^15 + X^^14 + X^^13 + X^^10 + X^^9 + X^^7 + X^^5 + X^^3 + 1
      25 -> X^^25 + X^^8 + X^^6 + X^^2 + 1
      26 -> X^^26 + X^^14 + X^^10 + X^^8 + X^^7 + X^^6 + X^^4 + X^^1 + 1
      27 -> X^^27 + X^^12 + X^^10 + X^^9 + X^^7 + X^^5 + X^^3 + X^^2 + 1
      28 -> X^^28 + X^^13 + X^^7 + X^^6 + X^^5 + X^^2 + 1
      29 -> X^^29 + X^^2 + 1
      30 -> X^^30 + X^^17 + X^^16 + X^^13 + X^^11 + X^^7 + X^^5 + X^^3 + X^^2 + X^^1 + 1
      31 -> X^^31 + X^^3 + 1
      32 -> X^^32 + X^^15 + X^^9 + X^^7 + X^^4 + X^^3 + 1
      _ ->
        error $ "The IrreduciblePoly instance for N2 included with the library (and exported by Crypto.Lol) only contains " ++
                "irreducible polynomials for characteristic-2 fields up to GF(2^^32). You need a polynomial " ++
                "for GF(2^^" ++ (show n) ++ "). Define your own instance of IrreduciblePoly and do " ++
                "not import Crypto.Lol."
