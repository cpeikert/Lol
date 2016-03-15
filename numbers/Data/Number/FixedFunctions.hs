-- Modified by Lennart Augustsson to fit into Haskell numerical hierarchy.
--
-- Module:
--
--      Fraction.hs
--
-- Language:
--
--      Haskell
--
-- Description: Rational with transcendental functionalities
--
--
--      This is a generalized Rational in disguise. Rational, as a type
--      synonim, could not be directly made an instance of any new class
--      at all.
--      But we would like it to be an instance of Transcendental, where
--      trigonometry, hyperbolics, logarithms, etc. are defined.
--      So here we are tiptoe-ing around, re-defining everything from
--      scratch, before designing the transcendental functions -- which
--      is the main motivation for this module.
--
--      Aside from its ability to compute transcendentals, Fraction
--      allows for denominators zero. Unlike Rational, Fraction does
--      not produce run-time errors for zero denominators, but use such
--      entities as indicators of invalid results -- plus or minus
--      infinities. Operations on fractions never fail in principle.
--
--      However, some function may compute slowly when both numerators
--      and denominators of their arguments are chosen to be huge.
--      For example, periodicity relations are utilized with large
--      arguments in trigonometric functions to reduce the arguments
--      to smaller values and thus improve on the convergence
--      of continued fractions. Yet, if pi number is chosen to
--      be extremely accurate then the reduced argument would
--      become a fraction with huge numerator and denominator
--      -- thus slowing down the entire computation of a trigonometric
--      function.
--
-- Usage:
--
--      When computation speed is not an issue and accuracy is important
--      this module replaces some of the functionalities typically handled
--      by the floating point numbers: trigonometry, hyperbolics, roots
--      and some special functions. All computations, including definitions
--      of the basic constants pi and e, can be carried with any desired
--      accuracy. One suggested usage is for mathematical servers, where
--      safety might be more important than speed. See also the module
--      Numerus, which supports mixed arithmetic between Integer,
--      Fraction and Cofra (Complex fraction), and returns complex
--      legal answers in some cases where Fraction would produce
--      infinities: log (-5), sqrt (-1), etc.
--
--
-- Required:
--
--      Haskell Prelude
--
-- Author:
--
--      Jan Skibinski, Numeric Quest Inc.
--
-- Date:
--
--      1998.08.16, last modified 2000.05.31
--
-- See also bottom of the page for description of the format used
-- for continued fractions, references, etc.
-------------------------------------------------------------------

module Data.Number.FixedFunctions where
import Prelude hiding (pi, sqrt, tan, atan, exp, log)
import Data.Ratio

approx      :: Rational -> Rational -> Rational
approx eps x = approxRational x eps

------------------------------------------------------------------
--              Category: Conversion
--      from continued fraction to fraction and vice versa,
--      from Taylor series to continued fraction.
-------------------------------------------------------------------
type CF = [(Rational, Rational)]

fromCF :: CF -> Rational
fromCF x =
        --
        -- Convert finite continued fraction to fraction
        -- evaluating from right to left. This is used
        -- mainly for testing in conjunction with "toCF".
        --
        foldr g 1 x
        where
            g :: (Rational, Rational) -> Rational -> Rational
            g u v = (fst u) + (snd u) / v

toCF :: Rational -> CF
toCF x =
        --
        -- Convert fraction to finite continued fraction
        --
        toCF' x []
        where
            toCF' u lst =
                case r of
                0 -> reverse (((q%1),(0%1)):lst)
                _ -> toCF' (b%r) (((q%1),(1%1)):lst)
                where
                    a = numerator u
                    b = denominator u
                    (q,r) = quotRem a b


approxCF :: Rational -> CF -> Rational
approxCF eps [] = 0
approxCF eps x
        --
        -- Approximate infinite continued fraction x by fraction,
        -- evaluating from left to right, and stopping when
        -- accuracy eps is achieved, or when a partial numerator
        -- is zero -- as it indicates the end of CF.
        --
        -- This recursive function relates continued fraction
        -- to rational approximation.
        --
        = approxCF' eps x 0 1 1 q' p' 1
            where
                h = fst (x!!0)
                (q', p') = x!!0
                approxCF' eps x v2 v1 u2 u1 a' n
                    | abs (1 - f1/f) < eps = approx eps f
                    | a == 0    = approx eps f
                    | otherwise = approxCF' eps x v1 v u1 u a (n+1)
                    where
                        (b, a) = x!!n
                        u  = b*u1 + a'*u2
                        v  = b*v1 + a'*v2
                        f  = u/v
                        f1 = u1/v1


-- Type signature determined by GHC.
fromTaylorToCF :: Fractional a => [a] -> a -> [(a, a)]
fromTaylorToCF s x =
        --
        -- Convert infinite number of terms of Taylor expansion of
        -- a function f(x) to an infinite continued fraction,
        -- where s = [s0,s1,s2,s3....] is a list of Taylor
        -- series coefficients, such that f(x)=s0 + s1*x + s2*x^2....
        --
        -- Require: No Taylor coefficient is zero
        --
        zero:one:[higher m | m <- [2..]]
        where
            zero      = (s!!0, s!!1 * x)
            one       = (1, -s!!2/s!!1 * x)
            higher m  = (1 + s!!m/s!!(m-1) * x, -s!!(m+1)/s!!m * x)


------------------------------------------------------------------
--                Category: Auxiliaries
------------------------------------------------------------------

fac :: Integer -> Integer
fac = product . enumFromTo 1

integerRoot2 :: Integer -> Integer
integerRoot2 1 = 1
integerRoot2 x =
        --
        -- Biggest integer m, such that x - m^2 >= 0,
        -- where x is a positive integer
        --
        integerRoot2' 0 x (x `div` 2) x
        where
            integerRoot2' lo hi r y
                | c > y      = integerRoot2' lo r ((r + lo) `div` 2) y
                | c == y     = r
                | otherwise  =
                    if (r+1)^2 > y then
                        r
                    else
                        integerRoot2' r hi ((r + hi) `div` 2) y
                    where c = r^2

-------------------------------------------------------------------
-- Everything below is the instantiation of class Transcendental
-- for type Rational. See also modules Cofra and Numerus.
--
--                Category: Constants
-------------------------------------------------------------------

pi :: Rational -> Rational
pi eps =
        --
        -- pi with accuracy eps
        --
        -- Based on Ramanujan formula, as described in Ref. 3
        -- Accuracy: extremely good, 10^-19 for one term of continued
        -- fraction
        --
        (sqrt eps d) / (approxCF eps (fromTaylorToCF s x))
        where
            x = 1%(640320^3)::Rational
            s = [((-1)^k*(fac (6*k))%((fac k)^3*(fac (3*k))))*((a*k+b)%c) | k<-[0..]]
            a = 545140134
            b = 13591409
            c = 426880
            d = 10005

---------------------------------------------------------------------
--                Category: Trigonometry
---------------------------------------------------------------------

tan :: Rational -> Rational -> Rational
tan eps 0  = 0
tan eps x
        --
        -- Tangent x computed with accuracy of eps.
        --
        -- Trigonometric identities are used first to reduce
        -- the value of x to a value from within the range of [-pi/2,pi/2]
        --
        | x >= half_pi'  = tan eps (x - ((1+m)%1)*xpi)
        | x <= -half_pi' = tan eps (x + ((1+m)%1)*xpi)
        --- | absx > 1       = 2 * t/(1 - t^2)
        | otherwise      = approxCF eps (cf x)
        where
            absx    = abs x
            t       = tan eps (x/2)
            m       = floor ((absx - half_pi)/ xpi)
            xpi     = pi eps
            half_pi'= 158%100
            half_pi = xpi * (1%2)
            cf u    = ((0%1,1%1):[((2*r + 1)/u, -1) | r <- [0..]])

sin :: Rational -> Rational -> Rational
sin eps 0      = 0
sin eps x      = 2*t/(1 + t*t)
        where
            t = tan eps (x/2)

cos :: Rational -> Rational -> Rational
cos eps 0      = 1
cos eps x      = (1 - p)/(1 + p)
        where
            t = tan eps (x/2)
            p = t*t

atan :: Rational -> Rational -> Rational
atan eps x
        --
        -- Inverse tangent of x with approximation eps
        --
        | x == 0       = 0
        | x > 1        =  (pi eps)/2 - atan eps (1/x)
        | x < -1       = -(pi eps)/2 - atan eps (1/x)
        | otherwise    = approxCF eps ((0,x):[((2*m - 1),(m*x)^2) | m<- [1..]])


asin :: Rational -> Rational -> Rational
asin eps x
        --
        -- Inverse sine of x with approximation eps
        --
        | x == 0    = 0
        | abs x > 1 = error "Fraction.asin"
        | x == 1    = (pi eps) *  (1%2)
        | x == -1   = (pi eps) * (-1%2)
        | otherwise = atan eps (x / (sqrt eps (1 - x^2)))


acos :: Rational -> Rational -> Rational
acos eps x
        --
        -- Inverse cosine of x with approximation eps
        --
        | x == 0    = (pi eps)*(1%2)
        | abs x > 1 = error "Fraction.sin"
        | x == 1    = 0
        | x == -1   = pi eps
        | otherwise = atan eps ((sqrt eps (1 - x^2)) / x)

---------------------------------------------------------------------
--                Category: Roots
---------------------------------------------------------------------

sqrt :: Rational -> Rational -> Rational
sqrt eps x
        --
        -- Square root of x with approximation eps
        --
        -- The CF pattern is: [(m,x-m^2),(2m,x-m^2),(2m,x-m^2)....]
        -- where m is the biggest integer such that x-m^2 >= 0
        --
        | x < 0        = error "Fraction.sqrt"
        | x == 0       = 0
        | x < 1        = 1/(sqrt eps (1/x))
        | otherwise    = approxCF eps ((m,x-m^2):[(2*m,x-m^2) | r<-[0..]])
        where
            m = (integerRoot2 (floor x))%1

---------------------------------------------------------------------
--              Category: Exponentials and hyperbolics
---------------------------------------------------------------------

exp :: Rational -> Rational -> Rational
exp eps x
        --
        -- Exponent of x with approximation eps
        --
        -- Based on Jacobi type continued fraction for exponential,
        -- with fractional terms:
        --     n == 0 ==> (1,x)
        --     n == 1 ==> (1 -x/2, x^2/12)
        --     n >= 2 ==> (1, x^2/(16*n^2 - 4))
        -- For x outside [-1,1] apply identity exp(x) = (exp(x/2))^2
        --
        | x == 0       = 1
        | x > 1        = (approxCF eps (f (x*(1%p))))^p
        | x < (-1)     = (approxCF eps (f (x*(1%q))))^q
        | otherwise    = approxCF eps (f x)
        where
            p = ceiling x
            q = -(floor x)
            f y = (1,y):(1-y/2,y^2/12):[(1,y^2/(16*n^2-4)) | n<-[2..]]


cosh :: Rational -> Rational -> Rational
cosh eps x =
        --
        -- Hyperbolic cosine with approximation eps
        --
        (a + b)*(1%2)
        where
            a = exp eps x
            b = 1/a

sinh :: Rational -> Rational -> Rational
sinh eps x =
        --
        -- Hyperbolic sine with approximation eps
        --
        (a - b)*(1%2)
        where
            a = exp eps x
            b = 1/a

tanh :: Rational -> Rational -> Rational
tanh eps x =
        --
        -- Hyperbolic tangent with approximation eps
        --
        (a - b)/ (a + b)
        where
            a = exp eps x
            b = 1/a

atanh :: Rational -> Rational -> Rational
atanh eps x
        --
        -- Inverse hyperbolic tangent with approximation eps
        --

--      | x >= 1     = 1%0
--      | x <= -1    = -1%0
        | otherwise  = (1%2) * (log eps ((1 + x) / (1 - x)))

asinh :: Rational -> Rational -> Rational
asinh eps x
        --
        -- Inverse hyperbolic sine
        --
--      | x == 1%0  =  1%0
--      | x == -1%0 = -1%0
        | otherwise  = log eps (x + (sqrt eps (x^2 + 1)))

acosh :: Rational -> Rational -> Rational
acosh eps x
        --
        -- Inverse hyperbolic cosine
        --
--      | x == 1%0 = 1%0
--      | x < 1     = 1%0
        | otherwise = log eps (x + (sqrt eps (x^2 - 1)))

---------------------------------------------------------------------
--                Category: Logarithms
---------------------------------------------------------------------

log :: Rational -> Rational -> Rational
log eps x
        --
        -- Natural logarithm of strictly positive x
        --
        -- Based on Stieltjes type continued fraction for log (1+y)
        --     (0,y):(1,y/2):[(1,my/(4m+2)),(1,(m+1)y/(4m+2)),....
        --     (m >= 1, two elements per m)
        -- Efficient only for x close to one. For larger x we recursively
        -- apply the identity log(x) = log(x/2) + log(2)
        --
        | x <= 0    = error "Fraction.log"
        | x <  1    = -log eps (1/x)
        | x == 1    =  0
        | otherwise =
            case (scaled (x,0)) of
            (1,s) -> (s%1) * approxCF eps (series 1)
            (y,0) -> approxCF eps (series (y-1))
            (y,s) -> approxCF eps (series (y-1)) + (s%1)*approxCF eps (series 1)
        where
            series :: Rational -> CF
            series u = (0,u):(1,u/2):[(1,u*((m+n)%(4*m + 2)))|m<-[1..],n<-[0,1]]
            scaled :: (Rational,Integer) -> (Rational, Integer)
            scaled (x, n)
                | x == 2 = (1,n+1)
                | x < 2 = (x, n)
                | otherwise = scaled (x*(1%2), n+1)


---------------------------------------------------------------------------
-- References:
--
-- 1. Classical Gosper notes on continued fraction arithmetic:
--      http:%www.inwap.com/pdp10/hbaker/hakmem/cf.html
-- 2. Pages on numerical constants represented as continued fractions:
--      http:%www.mathsoft.com/asolve/constant/cntfrc/cntfrc.html
-- 3. "Efficient on-line computation of real functions using exact floating
--     point", by Peter John Potts, Imperial College
--      http:%theory.doc.ic.ac.uk/~pjp/ieee.html
--------------------------------------------------------------------------

--------------------------------------------------------------------------

--      The following representation of continued fractions is used:
--
--      Continued fraction:         CF representation:
--      ==================           ====================
--      b0 + a0
--           -------        ==>      [(b0, a0), (b1, a1), (b2, a2).....]
--           b1 + a1
--                -------
--                b2 + ...
--
--      where "a's" and "b's" are Rationals.
--
--      Many continued fractions could be represented by much simpler form
--      [b1,b2,b3,b4..], where all coefficients "a" would have the same value 1
--      and would not need to be explicitely listed; and the coefficients "b"
--      could be chosen as integers.
--      However, there are some useful continued fractions that are
--      given with fraction coefficients: "a", "b" or both.
--      A fractional form can always be converted to an integer form, but
--      a conversion process is not always simple and such an effort is not
--      always worth of the achieved savings in the storage space or the
--      computational efficiency.
--
----------------------------------------------------------------------------
--
-- Copyright:
--
--      (C) 1998 Numeric Quest, All rights reserved
--
--      <jans@numeric-quest.com>
--
--      http://www.numeric-quest.com
--
-- License:
--
--      GNU General Public License, GPL
--
-----------------------------------------------------------------------------
