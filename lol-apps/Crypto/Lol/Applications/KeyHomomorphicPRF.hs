{-|
Module      : Crypto.Lol.Applications.KeyHomomorphicPRF
Description : Key-homomorphic PRF from <http://web.eecs.umich.edu/~cpeikert/pubs/kh-prf.pdf [BP14]>.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Key-homomorphic PRF from <http://web.eecs.umich.edu/~cpeikert/pubs/kh-prf.pdf [BP14]>.
-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module Crypto.Lol.Applications.KeyHomomorphicPRF
( FBTTop(..), FBT
, defaultFBT, genParams, genKey, prf
, Vector(..), replicate
) where

import Control.Applicative ((<$>))
import Control.Monad.Random hiding (fromList, split)
import Control.Monad.State
import Control.Monad.Reader

import Crypto.Lol hiding (replicate, modulus)

import Data.Singletons.TH
import Data.Maybe

import MathObj.Matrix hiding (zero)

singletons [d|

        -- FBTTop treated as kind, FBT treated as type
        data FBTTop = Leaf | Intern FBTTop FBTTop

        -- promote to type family for getting number of leaves
        sizeFBTTop :: FBTTop -> Pos
        sizeFBTTop Leaf = O
        sizeFBTTop (Intern l r) = (sizeFBTTop l) `addPos` (sizeFBTTop r)
        |]

-- type held at node is parameterized by tree topology
data FBT t n gad a where
    L :: BitStringMatrix 'Leaf a
      -> FBT 'Leaf n gad a
    I :: BitStringMatrix ('Intern l r) a -> FBT l n gad a -> FBT r n gad a
      -> FBT ('Intern l r) n gad a

newtype PRFKey n a = Key { unKey :: Matrix a }

data PRFParams n gad a = Params (Matrix a) (Matrix a)

-- | sized vector from blog post "Part 1: Dependent Types in Haskell"
data Vector n a where
    Lone :: a               -> Vector 'O a
    (:-) :: a -> Vector n a -> Vector ('S n) a
infixr 5 :-

deriving instance Show a => Show (Vector n a)

instance Eq a => Eq (Vector n a) where
    Lone a1  == Lone a2  = a1 == a2
    h1 :- t1 == h2 :- t2 = h1 == h2 && t1 == t2

-- | False, True
instance Enum (Vector 'O Bool) where
    toEnum x          = Lone (odd x)
    fromEnum (Lone x) = if x then 1 else 0

-- | Enumerates according to n-bit Gray Code
instance (PosC n, Enum (Vector n Bool)) => Enum (Vector ('S n) Bool) where
    toEnum = let thresh = 2^(posToInt $ fromSing (sing :: Sing n) :: Int)
                 modulus = 2 * thresh
             in  \x -> let x' = x `mod` modulus in
                 if x' < thresh
                 then False :- toEnum x'
                 else True  :- toEnum (modulus - 1 - x')
    fromEnum (x:-xs) = let modulus = 2 * 2^(posToInt $ fromSing (sing :: Sing n) :: Int)
                       in if x
                          then modulus - 1 - fromEnum xs
                          else fromEnum xs

-- | BitString definition
type BitString n = Vector n Bool

data BitStringMatrix t a
    = BSM { bsmBitString :: BitString (SizeFBTTop t), bsmMatrix :: Matrix a }

-- | Value held by m is inferred by compiler
split :: forall m n a . PosC m =>
            Vector (m `AddPos` n) a -> (Vector m a, Vector n a)
split (Lone _) = error "split: internal error"
split (h :- t) = case (sing :: Sing m) of
    SO    -> (Lone h, t)
    SS pm -> withSingI pm $ let (b, e) = split t in (h :- b, e)

value :: Vector 'O a -> a
value (Lone a) = a

replicate :: forall n a . PosC n => a -> Vector n a
replicate a = case (sing :: Sing n) of
    SO   -> Lone a
    SS n -> withSingI n $ a :- replicate a

root :: FBT t n gad a -> BitStringMatrix t a
root (L a)     = a
root (I a _ _) = a

subtrees :: FBT ('Intern l r) n gad a -> (FBT l n gad a, FBT r n gad a)
subtrees (I _ l r) = (l, r)


defaultFBT :: forall gad rq t n .
             (SingI t, Decompose gad rq, PosC (SizeFBTTop t))
           => PRFParams n gad rq
           -> FBT t n gad rq
defaultFBT p = updateFBT p Nothing $ replicate False

-- | Definition 2.1 in KHPRF paper [BP14]
-- Upon first evaluation, `Maybe FBT..` is `Nothing`. When updating an extant
-- evaluation, `Maybe FBT..` contains the current trees wrapped by Just.
-- Avoids redundant computation.
updateFBT :: forall gad rq t n . (SingI t, Decompose gad rq)
          => PRFParams n gad rq
          -> Maybe (FBT t n gad rq)
          -> BitString (SizeFBTTop t)
          -> FBT t n gad rq
updateFBT p@(Params a0 a1) fbt x = case (sing :: Sing t) of
    SLeaf       -> L $ BSM x $ if value x then a1 else a0
    SIntern _ _ | isJust fbt && x == bsmBitString (root $ fromJust fbt)
                -> fromJust fbt
    SIntern l r -> withSingI l $ withSingI r $ withSingI (sSizeFBTTop l) $
                   let (xl, xr) = split x
                       children = subtrees <$> fbt
                       fbtl = updateFBT p (fst <$> children) xl
                       fbtr = updateFBT p (snd <$> children) xr
                       al = bsmMatrix $ root fbtl
                       ar = bsmMatrix $ root fbtr
                       ar' = reduce <$> proxy (decomposeMatrix ar) (Proxy :: Proxy gad)
                   in  I (BSM x (al*ar')) fbtl fbtr


-- | Generate random r x c matrix
randomMtx :: (MonadRandom rnd, Random a) => Int -> Int -> rnd (Matrix a)
randomMtx r c = fromList r c <$> replicateM (r*c) getRandom

-- | Generate public initialization matrices A0 and A1
genParams :: forall gad rq rnd n .
            (MonadRandom rnd, Random rq, PosC n, Gadget gad rq)
          => rnd (PRFParams n gad rq)
genParams = let len = length $ untag (gadget :: Tagged gad [rq])
                n   = posToInt $ fromSing (sing :: Sing n)
            in do
                a0 <- randomMtx n $ n * len
                a1 <- randomMtx n $ n * len
                return $ Params a0 a1

-- | Generate private key vector s
genKey :: forall rq rnd n . (MonadRandom rnd, Random rq, PosC n)
       => rnd (PRFKey n rq)
genKey = fmap Key $ randomMtx 1 $ posToInt $ fromSing (sing :: Sing n)

-- | Equation 2.3 in KHPRF paper [BP14], monad-state version
prf :: forall gad rq rp t n m .
      (Rescale rq rp, Ring rq, SingI t, Decompose gad rq,
       MonadState (FBT t n gad rq) m,
       MonadReader (PRFParams n gad rq) m)
    => PRFKey n rq
    -> BitString (SizeFBTTop t)
    -> m (Matrix rp)
prf s x = do
    params <- ask
    modify (\fbt -> updateFBT params (Just fbt) x)
    fbt    <- get
    return $ let at = bsmMatrix $ root fbt
             in  rescale <$> (unKey s) * at

{-|
Note: Making `Vector` an instance of `Additive.C`
Option 1: Two separate instances for Vector `O Bool and Vector (`S n) Bool
    Recursive instance requires recursive restraint
    Use of `zero` would require extra `Additive.C` constraint in `defaultFBT`
Option 2: One instance, using singletons and `case` to distinguish
    Ugly syntax
    Didn't implement (functionality replaced by `replicate`)
    May need to do something similar for `Enum` if it causes errors
-}
