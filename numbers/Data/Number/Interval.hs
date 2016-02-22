-- | An incomplete implementation of interval aritrhmetic.
module Data.Number.Interval(Interval, ival, getIval) where

data Interval a = I a a

ival :: (Ord a) => a -> a -> Interval a
ival l h | l <= h = I l h
         | otherwise = error "Interval.ival: low > high"

getIval :: Interval a -> (a, a)
getIval (I l h) = (l, h)

instance (Ord a) => Eq (Interval a) where
    I l h == I l' h'  =  l == h' && h == l'
    I l h /= I l' h'  =  h < l' || h' < l

instance (Ord a) => Ord (Interval a) where
    I l h <  I l' h'  =  h <  l'
    I l h <= I l' h'  =  h <= l'
    I l h >  I l' h'  =  l >  h'
    I l h >= I l' h'  =  l >= h'
    -- These funcions are partial, so we just leave them out.
    compare _ _ = error "Interval compare"
    max _ _ = error "Interval max"
    min _ _ = error "Interval min"

instance (Eq a, Show a) => Show (Interval a) where
    showsPrec p (I l h) | l == h = showsPrec p l
                        | otherwise = showsPrec p l . showString ".." . showsPrec p h

instance (Ord a, Num a) => Num (Interval a) where
    I l h + I l' h'  =  I (l + l') (h + h')
    I l h - I l' h'  =  I (l - h') (h - l')
    I l h * I l' h'  =  I (minimum xs) (maximum xs) where xs = [l*l', l*h', h*l', h*h']
    negate (I l h)   =  I (-h) (-l)
    -- leave out abs and signum
    abs _ = error "Interval abs"
    signum _ = error "Interval signum"
    fromInteger i    =  I l l where l = fromInteger i
 
instance (Ord a, Fractional a) => Fractional (Interval a) where
    I l h / I l' h' | signum l' == signum h' && l' /= 0 =  I (minimum xs) (maximum xs)
                    | otherwise = error "Interval: division by 0"
                    where xs = [l/l', l/h', h/l', h/h']
    fromRational r   =  I l l where l = fromRational r
