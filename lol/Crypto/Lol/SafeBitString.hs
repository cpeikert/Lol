{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, GADTs,
             InstanceSigs, KindSignatures, PolyKinds, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- For safety, don't export the constructor.
module Crypto.Lol.SafeBitString
( SafeBitString(..)
) where

import Crypto.Lol.PosBinDefs

import qualified Data.BitString as B
import Data.Foldable
import Data.Word

data SafeBitString (n :: Pos) where
  Bit :: Bool -> SafeBitString O
  Cons :: Bool -> SafeBitString n -> SafeBitString (S n)

{-toList' :: SafeBitString bits n -> [Bool]
toList' (SafeBitString bits _) = B.toList bits

to01List' :: SafeBitString bits n -> [Word8]
to01List' (SafeBitString bits _) = B.to01List bits

-- Convenient constructor for the client.
from01List' :: [Word8] -> SafeBitString bits n
from01List' bits = SafeBitString (B.from01List bits) (length' bits)

-- An alternative constructor.
fromBitString :: B.BitString -> SafeBitString bits n
fromBitString bs = SafeBitString bs (length' $ B.toList bs)

-- Check: seems to be working.
isBit :: SafeBitString bits n -> Bool
isBit (SafeBitString _ len)
  | (len == O) = True
  | otherwise = False

-- Subtracts 1 from the result since our base case starts at 1.
-- Constantly inefficient :)
-- May need relocation (PodBinDefs.hs, perhaps)
length' :: Foldable t => t a -> Pos
length' foldable =
  let acc = (foldl' (\n _ -> (S n)) O foldable)
  in subPos acc O
-}
-- How to write the type signature to ensure type-safety?
--splitSBS :: (n ~ AddPos nl nr) => SafeBitString n -> (SafeBitString nl, SafeBitString nr)
--splitSBS (Cons b (Bit b')) = (Bit b, Bit b')
--splitSBS (Cons b rest) =
  --let (l,r) = splitSBS rest
  --in (Cons b l, r)
