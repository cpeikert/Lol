-- | Lazy natural numbers.
-- Addition and multiplication recurses over the first argument, i.e.,
-- @1 + n@ is the way to write the constant time successor function.
--
-- Note that (+) and (*) are not commutative for lazy natural numbers
-- when considering bottom.
module Data.Number.Natural(Natural, infinity) where

import Data.Maybe

data Natural = Z | S Natural

instance Show Natural where
    showsPrec p n = showsPrec p (toInteger n)

instance Eq Natural where
    x == y  =  x `compare` y == EQ

instance Ord Natural where
    Z   `compare` Z    =  EQ
    Z   `compare` S _  =  LT
    S _ `compare` Z    =  GT
    S x `compare` S y  =  x `compare` y

    -- (_|_) `compare` Z == _|_, but (_|_) >= Z = True
    -- so for maximum laziness, we need a specialized version of (>=) and (<=)
    _ >= Z = True
    Z >= S _ = False
    S a >= S b = a >= b

    (<=) = flip (>=)

    S x `max` S y = S (x `max` y)
    x   `max` y   = x + y

    S x `min` S y = S (x `min` y)
    _   `min` _   = Z

maybeSubtract :: Natural -> Natural -> Maybe Natural
a   `maybeSubtract` Z   = Just a
S a `maybeSubtract` S b = a `maybeSubtract` b
_   `maybeSubtract` _   = Nothing

instance Num Natural where
    Z   + y  =  y
    S x + y  =  S (x + y)

    x   - y  = fromMaybe (error "Natural: (-)") (x `maybeSubtract` y)

    Z   * y  =  Z
    S x * y  =  y + x * y

    abs x = x
    signum Z = Z
    signum (S _) = S Z

    fromInteger x | x < 0 = error "Natural: fromInteger"
    fromInteger 0 = Z
    fromInteger x = S (fromInteger (x-1))

instance Integral Natural where
    -- Not the most efficient version, but efficiency isn't the point of this module. :)
    quotRem x y =
        if x < y then
            (0, x)
        else
            let (q, r) = quotRem (x-y) y
            in  (1+q, r)
    div = quot
    mod = rem
    toInteger Z = 0
    toInteger (S x) = 1 + toInteger x

instance Real Natural where
    toRational = toRational . toInteger

instance Enum Natural where
    succ = S
    pred Z = error "Natural: pred 0"
    pred (S a) = a
    toEnum = fromIntegral
    fromEnum = fromIntegral
    enumFromThenTo from thn to | from <= thn = go from (to `maybeSubtract` from) where
      go from Nothing      = []
      go from (Just count) = from:go (step + from) (count `maybeSubtract` step)
      step = thn - from
    enumFromThenTo from thn to | otherwise = go (from + step) where
      go from | from >= to + step = let next = from - step in next:go next
              | otherwise         = []
      step = from - thn
    enumFrom a       = enumFromThenTo a (S a) infinity
    enumFromThen a b = enumFromThenTo a b infinity
    enumFromTo a c   = enumFromThenTo a (S a) c

-- | The infinite natural number.
infinity :: Natural
infinity = S infinity
