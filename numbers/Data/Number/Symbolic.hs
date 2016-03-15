-- | Symbolic number, i.e., these are not numbers at all, but just build
-- a representation of the expressions.
-- This implementation is incomplete in that it allows comnstruction,
-- but not deconstruction of the expressions.  It's mainly useful for
-- debugging.
module Data.Number.Symbolic(Sym, var, con, subst, unSym) where

import Data.Char(isAlpha)
import Data.Maybe(fromMaybe)

-- | Symbolic numbers over some base type for the literals.
data Sym a = Con a | App String ([a]->a) [Sym a]

instance (Eq a) => Eq (Sym a) where
    Con x      == Con x'        =  x == x'
    App f _ xs == App f' _ xs'  =  (f, xs) == (f', xs')
    _          == _             =  False

instance (Ord a) => Ord (Sym a) where
    Con x      `compare` Con x'        =  x `compare` x'
    Con _      `compare` App _ _ _     = LT
    App _ _ _  `compare` Con _         = GT
    App f _ xs `compare` App f' _ xs'  =  (f, xs) `compare` (f', xs')

-- | Create a variable.
var :: String -> Sym a
var s = App s undefined []

-- | Create a constant (useful when it is not a literal).
con :: a -> Sym a
con = Con

-- | The expression @subst x v e@ substitutes the expression @v@ for each
-- occurence of the variable @x@ in @e@.
subst :: (Num a, Eq a) => String -> Sym a -> Sym a -> Sym a
subst _ _ e@(Con _) = e
subst x v e@(App x' _ []) | x == x' = v
                          | otherwise = e
subst x v (App s f es) =
    case map (subst x v) es of
    [e] -> unOp (\ x -> f [x]) s e
    [e1,e2] -> binOp (\ x y -> f [x,y]) e1 s e2
    es' -> App s f es'

-- Turn a symbolic number into a regular one if it is a constant,
-- otherwise generate an error.
unSym :: (Show a) => Sym a -> a
unSym (Con c) = c
unSym e = error $ "unSym called: " ++ show e

instance (Show a) => Show (Sym a) where
    showsPrec p (Con c) = showsPrec p c
    showsPrec _ (App s _ []) = showString s
    showsPrec p (App op@(c:_) _ [x, y]) | not (isAlpha c) =
        showParen (p>q) (showsPrec ql x . showString op . showsPrec qr y)
        where (ql, q, qr) = fromMaybe (9,9,9) $ lookup op [
                   ("**", (9,8,8)),
                   ("/",  (7,7,8)),
                   ("*",  (7,7,8)),
                   ("+",  (6,6,7)),
                   ("-",  (6,6,7))]
    showsPrec p (App "negate" _ [x]) =
        showParen (p>=6) (showString "-" . showsPrec 7 x)
    showsPrec p (App f _ xs) =
        showParen (p>10) (foldl (.) (showString f) (map (\ x -> showChar ' ' . showsPrec 11 x) xs))

instance (Num a, Eq a) => Num (Sym a) where
    x + y         = binOp (+) x "+" y
    x - y         = binOp (-) x "-" y
    x * y         = binOp (*) x "*" y
    negate x      = unOp negate "negate" x
    abs    x      = unOp abs    "abs"    x
    signum x      = unOp signum "signum" x
    fromInteger x = Con (fromInteger x)

instance (Fractional a, Eq a) => Fractional (Sym a) where
    x / y          = binOp (/) x "/" y
    fromRational x = Con (fromRational x)

-- Assume the numbers are a field and simplify a little
binOp :: (Num a, Eq a) => (a->a->a) -> Sym a -> String -> Sym a -> Sym a
binOp f (Con x) _ (Con y) = Con (f x y)
binOp _ x "+" 0 = x
binOp _ 0 "+" x = x
binOp _ x "+" (App "+" _ [y, z]) = (x + y) + z
binOp _ x "+" y | isCon y && not (isCon x) = y + x
binOp _ x "+" (App "negate" _ [y]) = x - y
binOp _ x "-" 0 = x
binOp _ x "-" x' | x == x' = 0
binOp _ x "-" (Con y) | not (isCon x) = Con (-y) + x
binOp _ _ "*" 0 = 0
binOp _ x "*" 1 = x
binOp _ x "*" (-1) = -x
binOp _ 0 "*" _ = 0
binOp _ 1 "*" x = x
binOp _ (-1) "*" x = -x
binOp _ x "*" (App "*" _ [y, z]) = (x * y) * z
binOp _ x "*" y | isCon y && not (isCon x) = y * x
binOp _ x "*" (App "/" f [y, z]) = App "/" f [x*y, z]
{-
binOp _ x "*" (App "+" _ [y, z]) = x*y + x*z
binOp _ (App "+" _ [y, z]) "*" x = y*x + z*x
-}
binOp _ x "/" 1 = x
binOp _ x "/" (-1) = -x
binOp _ x "/" x' | x == x' = 1
binOp _ x "/" (App "/" f [y, z]) = App "/" f [x*z, y]
binOp f (App "**" _ [x, y]) "**" z = binOp f x "**" (y * z)
binOp _ _ "**" 0 = 1
binOp _ 0 "**" _ = 0
binOp f x op y = App op (\ [a,b] -> f a b) [x, y]

unOp :: (Num a) => (a->a) -> String -> Sym a -> Sym a
unOp f _ (Con c) = Con (f c)
unOp _ "negate" (App "negate" _ [x]) = x
unOp _ "abs" e@(App "abs" _ _) = e
unOp _ "signum" e@(App "signum" _ _) = e
unOp f op x = App op (\ [a] -> f a) [x]

isCon :: Sym a -> Bool
isCon (Con _) = True
isCon _ = False


instance (Integral a) => Integral (Sym a) where
    quot x y = binOp quot x "quot" y
    rem x y = binOp rem x "rem" y
    quotRem x y = (quot x y, rem x y)
    div x y = binOp div x "div" y
    mod x y = binOp mod x "mod" y
    toInteger (Con c) = toInteger c

instance (Enum a) => Enum (Sym a) where
    toEnum = Con . toEnum
    fromEnum (Con a) = fromEnum a

instance (Real a) => Real (Sym a) where
    toRational (Con c) = toRational c

instance (RealFrac a) => RealFrac (Sym a) where
    properFraction (Con c) = (i, Con c') where (i, c') = properFraction c

instance (Floating a, Eq a) => Floating (Sym a) where
    pi = var "pi"
    exp = unOp exp "exp"
    sqrt = unOp sqrt "sqrt"
    log = unOp log "log"
    x ** y = binOp (**) x "**" y
    logBase x y = binOp logBase x "logBase" y
    sin = unOp sin "sin"
    tan = unOp tan "tan"
    cos = unOp cos "cos"
    asin = unOp asin "asin"
    atan = unOp atan "atan"
    acos = unOp acos "acos"
    sinh = unOp sinh "sinh"
    tanh = unOp tanh "tanh"
    cosh = unOp cosh "cosh"
    asinh = unOp asinh "asinh"
    atanh = unOp atanh "atanh"
    acosh = unOp acosh "acosh"

instance (RealFloat a, Show a) => RealFloat (Sym a) where
    floatRadix = floatRadix . unSym
    floatDigits = floatDigits . unSym
    floatRange  = floatRange . unSym
    decodeFloat (Con c) = decodeFloat c
    encodeFloat m e = Con (encodeFloat m e)
    exponent (Con c) = exponent c
    exponent _ = 0
    significand (Con c) = Con (significand c)
    scaleFloat k (Con c) = Con (scaleFloat k c)
    scaleFloat _ x = x
    isNaN (Con c) = isNaN c
    isInfinite (Con c) = isInfinite c
    isDenormalized (Con c) = isDenormalized c
    isNegativeZero (Con c) = isNegativeZero c
    isIEEE = isIEEE . unSym
    atan2 x y = binOp atan2 x "atan2" y
