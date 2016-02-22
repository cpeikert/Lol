-- | A simple implementation of floating point numbers with a selectable
-- precision.  The number of digits in the mantissa is selected by the
-- 'Epsilon' type class from the "Fixed" module.
--
-- The numbers are stored in base 10.
module Data.Number.BigFloat(
    BigFloat(..),
    Epsilon(..), Eps1, EpsDiv10, Prec10, Prec50, PrecPlus20
    ) where

import Numeric(showSigned)
import Data.Number.Fixed
import qualified Data.Number.FixedFunctions as F

base :: (Num a) => a
base = 10

-- This representation is stupid, two Integers makes more sense,
-- but is more work.
-- | Floating point number where the precision is determined by the type /e/.
data BigFloat e = BF (Fixed e) Integer
    deriving (Eq)

instance (Epsilon e) => Show (BigFloat e) where
    showsPrec = showSigned showBF
      -- Assumes base is 10
      where showBF (BF m e) = showsPrec 0 m . showString "e" . showsPrec 0 e

instance (Epsilon e) => Num (BigFloat e) where
    BF m1 e1 + BF m2 e2  =  bf (m1' + m2') e
      where (m1', m2') = if e == e1 then (m1, m2 / base^(e-e2))
                                           else (m1 / base^(e-e1), m2)
            e = e1 `max` e2
    -- Do - via negate
    BF m1 e1 * BF m2 e2  =  bf (m1 * m2) (e1 + e2)
    negate (BF m e) = BF (-m) e
    abs (BF m e) = BF (abs m) e
    signum (BF m _) = bf (signum m) 0
    fromInteger i = bf (fromInteger i) 0

instance (Epsilon e) => Real (BigFloat e) where
    toRational (BF e m) = toRational e * base^^m

instance (Epsilon e) => Ord (BigFloat e) where
    compare x y = compare (toRational x) (toRational y)

instance (Epsilon e) => Fractional (BigFloat e) where
    recip (BF m e) = bf (base / m) (-(e + 1))
    -- Take care not to lose precision for small numbers
    fromRational x
      | x == 0 || abs x >= 1 = bf (fromRational x) 0
      | otherwise = recip $ bf (fromRational (recip x)) 0


-- normalizing constructor
-- XXX The scaling is very inefficient
bf :: (Epsilon e) => Fixed e -> Integer -> BigFloat e
bf m e | m == 0     = BF 0 0
       | m < 0      = - bf (-m) e
       | m >= base  = bf (m / base) (e + 1)
       | m < 1      = bf (m * base) (e - 1)
       | otherwise  = BF m e

instance (Epsilon e) => RealFrac (BigFloat e) where
    properFraction x@(BF m e) =
        if e < 0 then (0, x)
                 else let (i, f) = properFraction (m * base^^e)
                      in  (i, bf f 0)

instance (Epsilon e) => Floating (BigFloat e) where
    pi = bf pi 0
    sqrt = toFloat1 F.sqrt
    exp = toFloat1 F.exp
    log = toFloat1 F.log
    sin = toFloat1 F.sin
    cos = toFloat1 F.cos
    tan = toFloat1 F.tan
    asin = toFloat1 F.asin
    acos = toFloat1 F.acos
    atan = toFloat1 F.atan
    sinh = toFloat1 F.sinh
    cosh = toFloat1 F.cosh
    tanh = toFloat1 F.tanh
    asinh = toFloat1 F.asinh
    acosh = toFloat1 F.acosh
    atanh = toFloat1 F.atanh

instance (Epsilon e) => RealFloat (BigFloat e) where
    floatRadix _ = base
    floatDigits (BF m _) =
        floor $ logBase base $ recip $ fromRational $ precision m
    floatRange _ = (minBound, maxBound)
    decodeFloat x@(BF m e) =
        let d = floatDigits x
        in  (round $ m * base^d, fromInteger e - d)
    encodeFloat m e = bf (fromInteger m) (toInteger e)
    exponent (BF _ e) = fromInteger e
    significand (BF m _) = BF m 0
    scaleFloat n (BF m e) = BF m (e + toInteger n)
    isNaN _ = False
    isInfinite _ = False
    isDenormalized _ = False
    isNegativeZero _ = False
    isIEEE _ = False

toFloat1 :: (Epsilon e) => (Rational -> Rational -> Rational) ->
             BigFloat e -> BigFloat e
toFloat1 f x@(BF m e) =
    fromRational $ f (precision m * scl) (toRational m * scl)
      where scl = base^^e
