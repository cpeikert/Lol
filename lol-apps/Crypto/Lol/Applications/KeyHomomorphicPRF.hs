{-|
Module      : Crypto.Lol.Applications.KeyHomomorphicPRF
Description : Key-homomorphic PRF from <http://web.eecs.umich.edu/~cpeikert/pubs/kh-prf.pdf [BP14]>.
Copyright   : (c) Bogdan Manga, 2018
                  Chris Peikert, 2018
License     : GPL-3
Maintainer  : cpeikert@alum.mit.edu
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
( FBTTop(..), FBT, PRFKey, PRFParams
, prf, genKey, genParams, run, runT
, Vector, replicate, fromList, split
) where

import Control.Applicative ((<$>))
import Control.Monad.Random hiding (fromList, split)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Crypto.Lol hiding (replicate, head)
import Crypto.Lol.Reflects

import Data.Singletons.TH
import Data.Maybe

import qualified MathObj.Matrix as M

singletons [d|

        -- | Topology of a full binary tree (promoted to the type
        -- level by data kinds)
        data FBTTop = Leaf | Intern FBTTop FBTTop

        -- promote to type family for getting number of leaves
        sizeFBTTop :: FBTTop -> Pos
        sizeFBTTop Leaf = O
        sizeFBTTop (Intern l r) = (sizeFBTTop l) `addPos` (sizeFBTTop r)
             |]

-- | A full binary tree of topology @t@, which at each node stores a
-- bit string \( x \) of appropriate length (equal to the number of
-- leaves in the subtree rooted at the node) and a matrix having @n@
-- rows over ring @r@; the matrix is \( A_T(x) \) for the gadget
-- indicated by @gad@.
data FBT t n gad a where
  L :: BitStringMatrix 'Leaf a
    -> FBT 'Leaf n gad a
  I :: BitStringMatrix ('Intern l r) a -> FBT l n gad a -> FBT r n gad a
    -> FBT ('Intern l r) n gad a

-- | A PRF secret key of dimension @n@ over ring @a@.
newtype PRFKey n a = Key { unKey :: Matrix a }

-- | PRF public parameters for an @n@-dimension secret key over @a@,
-- using a gadget indicated by @gad@.
data PRFParams n gad a = Params (Matrix a) (Matrix a)

-- | A 'BitString' together with a 'Matrix.T'
data BitStringMatrix t a
  = BSM { bsmBitString :: BitString (SizeFBTTop t), bsmMatrix :: Matrix a }

-- | The value stored at the root of a full binary tree.
root :: FBT t n gad a -> BitStringMatrix t a
root (L a)     = a
root (I a _ _) = a

subtrees :: FBT ('Intern l r) n gad a -> (FBT l n gad a, FBT r n gad a)
subtrees (I _ l r) = (l, r)

-- | Compute \( \mathbf{A}_T(x) \) from Definition 2.1 of [BP14],
-- given public parameters, the input \( x \), and (optionally) an
-- 'FBT' from a previous call, to amortize the computation across many
-- inputs.
updateFBT :: forall gad rq t n . (SingI t, Decompose gad rq)
          => PRFParams n gad rq
          -> Maybe (FBT t n gad rq)
          -> BitString (SizeFBTTop t)
          -> FBT t n gad rq
updateFBT p@(Params a0 a1) fbt x = case (sing :: Sing t) of
    SLeaf       -> L $ BSM x $ if head x then a1 else a0
    SIntern _ _  | isJust fbt && x == bsmBitString (root $ fromJust fbt)
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

-- | A random matrix having a given number of rows and columns.
randomMtx :: (MonadRandom rnd, Random a) => Int -> Int -> rnd (Matrix a)
randomMtx r c = M.fromList r c <$> replicateM (r*c) getRandom

-- | Generate public parameters (\( \mathbf{A}_0 \) and \(
-- \mathbf{A}_1 \)) for @n@-dimensional secret keys over a ring @rq@
-- for gadget indicated by @gad@.
genParams :: forall gad rq rnd n .
            (MonadRandom rnd, Random rq, Reflects n Int, Gadget gad rq)
          => rnd (PRFParams n gad rq)
genParams = let len = length $ untag (gadget :: Tagged gad [rq])
                n   = proxy value (Proxy :: Proxy n)
            in do
                a0 <- randomMtx n $ n * len
                a1 <- randomMtx n $ n * len
                return $ Params a0 a1

-- | Generate an @n@-dimensional secret key over @rq@.
genKey :: forall rq rnd n . (MonadRandom rnd, Random rq, Reflects n Int)
       => rnd (PRFKey n rq)
genKey = fmap Key $ randomMtx 1 $ proxy value (Proxy :: Proxy n)

-- | Given a secret key and a PRF input, compute the PRF output. The
-- output is in a monadic context that needs to be able to access
-- 'PRFParams' public parameters and to keep an 'FBT' as state for
-- efficient amortization across calls.
prf :: (Rescale rq rp, Decompose gad rq, SingI t,
        MonadState (FBT t n gad rq) m, MonadReader (PRFParams n gad rq) m)
    => PRFKey n rq              -- | secret key
    -> BitString (SizeFBTTop t) -- | input \( x \)
    -> m (Matrix rp)            -- | PRF output
prf s x = do
  p <- ask
  modify (\fbt -> updateFBT p (Just fbt) x)
  fbt <- get
  return $ let at = bsmMatrix $ root fbt in  rescale <$> (unKey s) * at

-- | Run a PRF computation with some public parameters.
-- E.g.: @run top params (prf key x)@
run :: (Decompose gad rq, SingI t, PosC (SizeFBTTop t))
  => proxy t                       -- | proxy for tree topology
  -> PRFParams n gad rq            -- | public parameters
  -> StateT (FBT t n gad rq) (Reader (PRFParams n gad rq)) a
                                   -- | prf computation
  -> a
run pt p = runIdentity . runT pt p

-- | More general (monad transformer) version of 'run'.
runT :: (Decompose gad rq, SingI t, PosC (SizeFBTTop t), Monad m)
  => proxy t
  -> PRFParams n gad rq
  -> StateT (FBT t n gad rq) (ReaderT (PRFParams n gad rq) m) a
  -> m a
runT _ p = flip runReaderT p .
           (flip evalStateT $ updateFBT p Nothing (replicate False))

-- | Type-safe sized vector from blog post "Part 1: Dependent Types in
-- Haskell"
data Vector n a where
  Lone :: a               -> Vector 'O a
  (:-) :: a -> Vector n a -> Vector ('S n) a

infixr 5 :-

deriving instance Show a => Show (Vector n a)

instance Eq a => Eq (Vector n a) where
  Lone a1  == Lone a2  = a1 == a2
  h1 :- t1 == h2 :- t2 = h1 == h2 && t1 == t2

-- | Enumerates according to the n-bit Gray code, starting with all 'False'
instance PosC n => Enum (Vector n Bool) where
  toEnum = case (sing :: Sing n) of
             SO   -> Lone . odd
             SS m -> withSingI m $
                     let thresh = 2^(sPosToInt m)
                         modulus = 2 * thresh
                     in  \x -> let x' = x `mod` modulus
                               in if x' < thresh
                                  then False :- toEnum x'
                                  else True  :- toEnum (modulus - 1 - x')
  fromEnum = case (sing :: Sing n) of
               SO   -> \(Lone x) -> if x then 1 else 0
               SS m -> withSingI m $
                       let modulus :: Int = 2^(1 + sPosToInt m)
                       in \(x:-xs) -> if x
                                      then modulus - 1 - fromEnum xs
                                      else fromEnum xs

instance (PosC n) => Enumerable (Vector n Bool) where
  values = case (sing :: Sing n) of
    SO   -> [Lone False, Lone True]
    SS m -> withSingI m $
            let num = 2^(1 + sPosToInt m)
            in  take num [replicate False ..]

-- | An @n@-dimensional 'Vector' of 'Bool's
type BitString n = Vector n Bool

head :: Vector n a -> a
head (Lone a) = a
head (a :- _) = a

-- | Split a 'Vector' into two
split :: forall m n a . PosC m
      => Vector (m `AddPos` n) a -> (Vector m a, Vector n a)
split (h :- t) = case (sing :: Sing m) of
  SO    -> (Lone h, t)
  SS pm -> withSingI pm $ let (b, e) = split t in (h :- b, e)
split (Lone _) = error "split: internal error; can't split a Lone"

-- | Create a 'Vector' full of given value
replicate :: forall n a . PosC n => a -> Vector n a
replicate a = case (sing :: Sing n) of
  SO   -> Lone a
  SS n -> withSingI n $ a :- replicate a


-- | Convert a list to a 'Vector', return 'Nothing' if lengths don't match
fromList :: forall n a . PosC n => [a] -> Maybe (Vector n a)
fromList xs = case (sing :: Sing n) of
  SO   -> case xs of
            (x:[])   -> Just (Lone x)
            _        -> Nothing
  SS n -> withSingI n $ case xs of
            (x:rest) -> (:-) x <$> fromList rest
            _        -> Nothing

sPosToInt :: SPos n -> Int
sPosToInt SO     = 1
sPosToInt (SS a) = 1 + sPosToInt a


{-|
Note: Making 'Vector' an instance of 'Additive.C'
Option 1: Two separate instances for Vector `O Bool and Vector (`S n) Bool
    Recursive instance requires recursive restraint
    Use of `zero` would require extra 'Additive.C' constraint in 'defaultFBT'
Option 2: One instance, using singletons and 'case' to distinguish
    Ugly syntax
    Didn't implement (functionality replaced by 'replicate')
    May need to do something similar for 'Enum' if it causes errors
-}
