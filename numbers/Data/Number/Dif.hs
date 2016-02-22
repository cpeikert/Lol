-- | The 'Data.Number.Dif' module contains a data type, 'Dif', that allows for
-- automatic forward differentiation.
--
-- All the ideas are from Jerzy Karczmarczuk\'s work,
-- see <http://users.info.unicaen.fr/~karczma/arpap/diffalg.pdf>.
--
-- A simple example, if we define
--
-- > foo x = x*x
--
-- then the function
--
-- > foo' = deriv foo
--
-- will behave as if its body was 2*x.
--
module Data.Number.Dif(Dif, val, df, mkDif, dCon, dVar, deriv, unDif) where

-- |The 'Dif' type is the type of differentiable numbers.
-- It's an instance of all the usual numeric classes.
-- The computed derivative of a function is is correct
-- except where the function is discontinuous, at these points
-- the derivative should be a Dirac pulse, but it isn\'t.
--
-- The 'Dif' numbers are printed with a trailing ~~ to
-- indicate that there is a \"tail\" of derivatives.
data Dif a = D !a (Dif a) | C !a

-- |The 'dCon' function turns a normal number into a 'Dif'
-- number with the same value.  Not that numeric literals
-- do not need an explicit conversion due to the normal
-- Haskell overloading of literals.
dCon :: (Num a) => a -> Dif a
dCon x = C x

-- |The 'dVar' function turns a number into a variable
-- number.  This is the number with with respect to which
-- the derivaticve is computed.
dVar :: (Num a, Eq a) => a -> Dif a
dVar x = D x 1

-- |The 'df' takes a 'Dif' number and returns its first
-- derivative.  The function can be iterated to to get
-- higher derivaties.
df :: (Num a, Eq a) => Dif a -> Dif a
df (D _ x') = x'
df (C _   ) = 0

-- |The 'val' function takes a 'Dif' number back to a normal
-- number, thus forgetting about all the derivatives.
val :: Dif a -> a
val (D x _) = x
val (C x  ) = x

-- |The 'mkDif' takes a value and 'Dif' value and makes
-- a 'Dif' number that has the given value as its normal
-- value, and the 'Dif' number as its derivatives.
mkDif :: a -> Dif a -> Dif a
mkDif = D

-- |The 'deriv' function is a simple utility to take the
-- derivative of a (single argument) function.
-- It is simply defined as
--
-- >  deriv f = val . df . f . dVar
--
deriv :: (Num a, Num b, Eq a, Eq b) => (Dif a -> Dif b) -> (a -> b)
deriv f = val . df . f . dVar

-- |Convert a 'Dif' function to an ordinary function.
unDif :: (Num a, Eq a) => (Dif a -> Dif b) -> (a -> b)
unDif f = val . f . dVar

instance (Show a) => Show (Dif a) where
    show x = show (val x) ++ "~~"

instance (Read a) => Read (Dif a) where
    readsPrec p s = [(C x, s') | (x, s') <- readsPrec p s]

instance (Eq a) => Eq (Dif a) where
    x == y  =  val x == val y

instance (Ord a) => Ord (Dif a) where
    x `compare` y  =  val x `compare` val y

instance (Num a, Eq a) => Num (Dif a) where
    (C x)    + (C y)         =  C (x + y)
    (C x)    + (D y y')      =  D (x + y) y'
    (D x x') + (C y)         =  D (x + y) x'
    (D x x') + (D y y')      =  D (x + y) (x' + y')

    (C x)    - (C y)         =  C (x - y)
    (C x)    - (D y y')      =  D (x - y) (-y')
    (D x x') - (C y)         =  D (x - y) x'
    (D x x') - (D y y')      =  D (x - y) (x' - y')

    (C 0)      * _           =  C 0
    _          * (C 0)       =  C 0
    (C x)      * (C y)       =  C (x * y)
    p@(C x)    * (D y y')    =  D (x * y) (p * y')
    (D x x')   * q@(C y)     =  D (x * y) (x' * q)
    p@(D x x') * q@(D y y')  =  D (x * y) (x' * q + p * y')

    negate (C x)             =  C (negate x)
    negate (D x x')          =  D (negate x) (negate x')

    fromInteger i            =  C (fromInteger i)

    abs (C x)                =  C (abs x)
    abs p@(D x x')           =  D (abs x) (signum p * x')

    -- The derivative of the signum function is (2*) the Dirac impulse,
    -- but there's not really any good way to encode this.
    -- We could do it by +Infinity (1/0) at 0.
    signum (C x)             =  C (signum x)
    signum (D x _)           =  C (signum x)

instance (Fractional a, Eq a) => Fractional (Dif a) where
    recip (C x)    = C (recip x)
    recip (D x x') = ip
        where ip = D (recip x) (-x' * ip * ip)
    fromRational r = C (fromRational r)

lift :: (Num a, Eq a) => [a -> a] -> Dif a -> Dif a
lift (f : _) (C x) = C (f x)
lift (f : f') p@(D x x') = D (f x) (x' * lift f' p)
lift _ _ = error "lift"

instance (Floating a, Eq a) => Floating (Dif a) where
    pi               = C pi

    exp (C x)        = C (exp x)
    exp (D x x')     = r where r = D (exp x) (x' * r)

    log (C x)        = C (log x)
    log p@(D x x')   = D (log x) (x' / p)

    sqrt (C x)       = C (sqrt x)
    sqrt (D x x')    = r where r = D (sqrt x) (x' / (2 * r))

    sin              = lift (cycle [sin, cos, negate . sin, negate . cos])
    cos              = lift (cycle [cos, negate . sin, negate . cos, sin])

    acos (C x)       = C (acos x)
    acos p@(D x x')  = D (acos x) (-x' / sqrt(1 - p*p))
    asin (C x)       = C (asin x)
    asin p@(D x x')  = D (asin x) ( x' / sqrt(1 - p*p))
    atan (C x)       = C (atan x)
    atan p@(D x x')  = D (atan x) ( x' / (p*p - 1))

    sinh x           = (exp x - exp (-x)) / 2
    cosh x           = (exp x + exp (-x)) / 2
    asinh x          = log (x + sqrt (x*x + 1))
    acosh x          = log (x + sqrt (x*x - 1))
    atanh x          = (log (1 + x) - log (1 - x)) / 2

instance (Real a) => Real (Dif a) where
    toRational = toRational . val

instance (RealFrac a) => RealFrac (Dif a) where
    -- Second component should have an impulse derivative.
    properFraction x = (i, x - fromIntegral i) where (i, _) = properFraction (val x)
    truncate = truncate . val
    round    = round    . val
    ceiling  = ceiling  . val
    floor    = floor    . val

-- Partial definition on purpose, more could be defined.
instance (RealFloat a) => RealFloat (Dif a) where
    floatRadix = floatRadix . val
    floatDigits = floatDigits . val
    floatRange  = floatRange . val
    exponent _ = 0
    scaleFloat 0 x = x
    isNaN = isNaN . val
    isInfinite = isInfinite . val
    isDenormalized = isDenormalized . val
    isNegativeZero = isNegativeZero . val
    isIEEE = isIEEE . val
    -- Set these to undefined rather than omit them to avoid compiler
    -- warnings.
    decodeFloat = undefined
    encodeFloat = undefined
