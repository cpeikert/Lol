{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MagicHash, MultiParamTypeClasses,
             ScopedTypeVariables, TypeFamilies, TypeOperators, UnboxedTuples, UndecidableInstances #-}

module HPFloat (Prec, BigFloat) where

import qualified Algebra.Absolute       as Absolute
import qualified Algebra.Additive       as Additive
import qualified Algebra.Algebraic      as Algebraic
import qualified Algebra.Field          as Field
import qualified Algebra.RealField      as RealField
import qualified Algebra.RealRing       as RealRing
import qualified Algebra.Ring           as Ring
import qualified Algebra.Transcendental as Transcendental

import Data.Bits ((.|.),shiftR,shiftL)
import Data.Functor.Trans.Tagged (proxy)
import Data.Int
import Data.List (unfoldr)
import Data.Number.BigFloat
import Data.Number.Fixed
import Data.Proxy
import Data.Ratio
import Data.Array.Repa.Eval         as R
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Storable as S
import Data.Word (Word8)

import Foreign.Ptr (plusPtr, castPtr, Ptr)
import Foreign.Storable
import Foreign.Storable.Tuple
import GHC.Prim (touch#)
import GHC.TypeLits
import GHC.Types (IO(..))
import Prelude as P
import System.Random

digitsToBytes :: Integer -> Int
digitsToBytes 0 = 1
digitsToBytes d = ceiling $ logBase 2 (2*((10^d)-1)+1)

-- taken from haskoin
-- | Decode a big endian Integer from a bytestring
fromByteArray :: [Word8] -> Integer
fromByteArray = (foldr f 0)
  where f w n = (toInteger w) .|. shiftL n 8

-- taken from haskoin
-- | Encode an Integer to a bytestring as big endian
toByteArray :: Integer -> [Word8]
toByteArray 0 = [0]
toByteArray i 
    | i > 0     = unfoldr f i
    | otherwise = error "toByteArray not defined for negative values"
  where 
    f 0 = Nothing
    f x = Just $ (fromInteger x :: Word8, x `shiftR` 8)

main = do
  let y@(BF x e) = 0 :: BigFloat (Prec 10)

  print x
  print e
  print y
  print $ S.head $ S.singleton y

-- | /n/-bit mantissa.
data Prec (n :: Nat)
instance (KnownNat n) => Epsilon (Prec n) where
    eps _ = 10^^(-(fromInteger $  natVal (Proxy::Proxy n)))




instance (Epsilon (Prec n)) => Additive.C (BigFloat (Prec n)) where
  (+) = (P.+)
  (-) = (P.-)
  zero = fromRational 0

instance (Epsilon (Prec n)) => Ring.C (BigFloat (Prec n)) where
  (*) = (P.*)
  fromInteger = P.fromInteger

instance (Epsilon (Prec n)) => Field.C (BigFloat (Prec n)) where
  (/) = (P./)
  recip = P.recip

instance (Epsilon (Prec n)) => Algebraic.C (BigFloat (Prec n)) where
  sqrt = P.sqrt
  root n x = x P.** (fromRational $ 1 % n :: BigFloat (Prec n))  

instance (Epsilon (Prec n)) => Transcendental.C (BigFloat (Prec n)) where
  pi = P.pi
  exp = P.exp
  log = P.log
  sin = P.sin
  cos = P.cos
  tan = P.tan
  asin = P.asin
  acos = P.acos
  atan = P.atan
  sinh = P.sinh
  cosh = P.cosh
  tanh = P.tanh
  asinh = P.asinh
  acosh = P.acosh
  atanh = P.atanh

instance (Epsilon (Prec n)) => RealRing.C (BigFloat (Prec n)) where
  floor = Ring.fromInteger . (P.floor :: BigFloat (Prec n) -> Integer)

instance (Epsilon (Prec n)) => RealField.C (BigFloat (Prec n))

instance (Epsilon (Prec n)) => Absolute.C (BigFloat (Prec n)) where
  abs = P.abs
  signum = P.signum

-- outputs random value in [0,1]
instance (Epsilon (Prec n), KnownNat n) => Random (BigFloat (Prec n)) where
  random = error "`random` not defined for BigFloat. Use `randomR` instead."
  randomR (0,1) g = 
    let n = natVal (Proxy :: Proxy n)
        (num,g') = randomR (0,10^n) g
    in (fromRational $ num % (10^n), g')

-- note: Fixed (Prec n) is *not* Storable
instance (KnownNat n, Storable (BFRepr n), Flatten (NBitRepr n)) => Storable (BigFloat (Prec n)) where
  sizeOf _ = sizeOf (undefined :: (BFRepr n))
  alignment _ = alignment (undefined :: (BFRepr n))
  peek p = serialToBF <$> peek (castPtr p)
  poke p = poke (castPtr p) . bfToSerial

sanityCheck :: forall n . (Epsilon (Prec n), KnownNat n) => Fixed (Prec n) -> Integer -> Bool
sanityCheck f e = 
  let r = toRational f
      num = numerator r
      den = denominator r
      numBytes = digitsToBytes $ natVal (Proxy :: Proxy n)
      numWords = toByteArray $ numerator (abs r)
      denWords = toByteArray $ denominator r
      e' = fromInteger e :: Int32
  in (den > 0) && (length numWords <= numBytes) && (length denWords <= numBytes) && (e == fromIntegral e')

type BFRepr n = (Bool, (NBitRepr n), (NBitRepr n), Int32)
type NBitRepr n = Nest (CeilLog256 (10^(n+1)) 256 ((10^(n+1))<=?255) 1) Word8
type family CeilLog256 x y xlty words where
  CeilLog256 x y True words = words
  CeilLog256 x y False words = CeilLog256 x (y*256) (x<=?((y*256)-1)) (words+1)

type family Nest n t where
  Nest 1 t = t
  Nest n t = (t,Nest (n-1) t)

newtype instance U.MVector s (BigFloat (Prec n)) = MV_BigFloat (U.MVector s (BFRepr n))
newtype instance U.Vector (BigFloat (Prec n)) = V_BigFloat (U.Vector (BFRepr n))

-- Unbox, when underlying representation is
instance (U.Unbox (BFRepr n), KnownNat n, Flatten (NBitRepr n)) 
  => U.Unbox (BigFloat (Prec n))

instance (U.Unbox (BFRepr n), KnownNat n, Flatten (NBitRepr n)) 
  => M.MVector U.MVector (BigFloat (Prec n)) where
  basicLength (MV_BigFloat v) = M.basicLength v
  basicUnsafeSlice z n (MV_BigFloat v) = MV_BigFloat $ M.basicUnsafeSlice z n v
  basicOverlaps (MV_BigFloat v1) (MV_BigFloat v2) = M.basicOverlaps v1 v2
  basicInitialize (MV_BigFloat v) = M.basicInitialize v
  basicUnsafeNew n = MV_BigFloat <$> M.basicUnsafeNew n
  basicUnsafeReplicate n x = MV_BigFloat <$> M.basicUnsafeReplicate n (bfToSerial x)
  basicUnsafeRead (MV_BigFloat v) z = serialToBF <$> M.basicUnsafeRead v z
  basicUnsafeWrite (MV_BigFloat v) z x = M.basicUnsafeWrite v z (bfToSerial x)
  basicClear (MV_BigFloat v) = M.basicClear v
  basicSet (MV_BigFloat v) x = M.basicSet v (bfToSerial x)
  basicUnsafeCopy (MV_BigFloat v1) (MV_BigFloat v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_BigFloat v1) (MV_BigFloat v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_BigFloat v) n = MV_BigFloat <$> M.basicUnsafeGrow v n

instance (U.Unbox (BFRepr n), KnownNat n, Flatten (NBitRepr n)) 
  => G.Vector U.Vector (BigFloat (Prec n)) where
  basicUnsafeFreeze (MV_BigFloat v) = V_BigFloat <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_BigFloat v) = MV_BigFloat <$> G.basicUnsafeThaw v
  basicLength (V_BigFloat v) = G.basicLength v
  basicUnsafeSlice z n (V_BigFloat v) = V_BigFloat $ G.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_BigFloat v) z = serialToBF <$> G.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_BigFloat mv) (V_BigFloat v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

instance Elt Integer where
  {-# INLINE touch #-}
  touch b = IO (\state -> case touch# b state of
                        state' -> (# state', () #))
  zero = 0
  one = 1

instance R.Elt Rational where
  touch rat = do
    touch $ numerator rat
    touch $ denominator rat
  zero = 0
  one = 1

instance (KnownNat n) => R.Elt (Fixed (Prec n)) where
  touch f = touch $ toRational f
  zero = 0
  one = 1

instance (KnownNat n) => R.Elt (BigFloat (Prec n)) where
    touch (BF f e) = do
        touch f
        touch e
    zero = zero
    one = one

serialToBF :: (KnownNat n, Flatten (NBitRepr n)) 
  => BFRepr n -> BigFloat (Prec n)
serialToBF (isNeg, numRepr, denRepr, e) = 
  let numWords = flatten numRepr
      denWords = flatten denRepr
      num' = fromByteArray numWords
      den = fromByteArray denWords
      num = if isNeg then -num' else num'
      rat = fromRational $ num % den
  in BF rat $ fromIntegral e

bfToSerial :: forall n . (KnownNat n, Flatten (NBitRepr n)) 
  => BigFloat (Prec n) -> BFRepr n
bfToSerial (BF f e) =
  let rat = toRational f
      num = abs $ numerator rat
      den = denominator rat
      numBytes = digitsToBytes $ natVal (Proxy :: Proxy n)
      pad xs = take numBytes $ xs ++ (repeat 0)
      numWords = pad $ toByteArray num
      denWords = pad $ toByteArray den
      numRepr = nest numWords
      denRepr = nest denWords
      isNeg = if numerator rat < 0 then True else False
     --EAC: remove this eventually
  in if sanityCheck f e 
     then (isNeg, numRepr, denRepr, fromInteger e)
     else error "failed sanity check"

class Flatten t where
  flatten :: t -> [Word8]
  nest :: [Word8] -> t

instance Flatten Word8 where
  flatten = pure
  nest = head

instance (Flatten c) => Flatten (Word8, c) where
  flatten (a,b) = a : flatten b
  nest (a : xs) = (a, nest xs)
