{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, GADTs,
             InstanceSigs, KindSignatures, PolyKinds, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- For safety, don't export the constructor.
module Crypto.Lol.SafeBitString
( SafeBitString(..)
) where

import Crypto.Lol.PosBinDefs

data SafeBitString (n :: Pos) where
  Bit :: Bool -> SafeBitString O
  Cons :: Bool -> SafeBitString n -> SafeBitString (S n)

-- | Splits the SafeBitString into two separate SafeBitStrings.
{-splitSBS :: (n ~ (AddPos nl nr)) => SafeBitString n -> (SafeBitString nl, SafeBitString nr)
splitSBS (Cons b (Bit b')) = (Bit b, Bit b')
splitSBS (Cons b rest) =
  let (l,r) = splitSBS rest
  in (Cons b l, r)-}
