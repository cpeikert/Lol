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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Lol.Applications.KeyHomomorphicPRF
( FBT(..), SFBT, SizeFBT, FBTC, singFBT
, PRFKey, PRFParams, PRFState
, genKey, genParams, prf, prfState, prfAmortized, run, runT
, Vector, BitString
, replicate, replicateS, fromList, fromListS, split, splitS
) where

import Control.Applicative    ((<$>), (<*>))
import Control.Monad.Identity
import Control.Monad.Random   hiding (fromList, split)
import Control.Monad.State

import Crypto.Lol          hiding (head, replicate)
import Crypto.Lol.Reflects

import Data.Maybe
import Data.Singletons.TH

import qualified MathObj.Matrix as M

singletons [d|

        -- | A full binary tree (promoted to the type level by data
        -- kinds, and singleton-ized): each node is either a leaf or
        -- has two children.
        data FBT = Leaf | Intern FBT FBT

        -- promote to type family for getting number of leaves
        sizeFBT :: FBT -> Pos
        sizeFBT Leaf         = O
        sizeFBT (Intern l r) = (sizeFBT l) `addPos` (sizeFBT r)
             |]

-- | Kind-restricted type synonym for 'SingI'
type FBTC (t :: FBT) = SingI t

-- | Kind-restricted synonym for 'sing'
singFBT :: FBTC t => SFBT t
singFBT = sing

-- | A PRF secret key of dimension @n@ over ring @a@.
newtype PRFKey n a = Key { key :: Matrix a }

-- | Generate an @n@-dimensional secret key over @rq@.
genKey :: forall rq rnd n . (MonadRandom rnd, Random rq, Reflects n Int)
       => rnd (PRFKey n rq)
genKey = fmap Key $ randomMtx 1 $ value @n

-- | PRF public parameters for an @n@-dimension secret key over @a@,
-- using a gadget indicated by @gad@.
data PRFParams n gad a = Params { a0 :: (Matrix a), a1 :: (Matrix a) }

-- | Generate public parameters (\( \mathbf{A}_0 \) and \(
-- \mathbf{A}_1 \)) for @n@-dimensional secret keys over a ring @rq@
-- for gadget indicated by @gad@.
genParams :: forall gad rq rnd n .
            (MonadRandom rnd, Random rq, Reflects n Int, Gadget gad rq)
          => rnd (PRFParams n gad rq)
genParams = let len = length $ gadget @gad @rq
                n   = value @n
            in Params <$> (randomMtx n (n*len)) <*> (randomMtx n (n*len))

-- | A random matrix having a given number of rows and columns.
randomMtx :: (MonadRandom rnd, Random a) => Int -> Int -> rnd (Matrix a)
randomMtx r c = M.fromList r c <$> replicateM (r*c) getRandom

-- | PRF state for tree topology @t@ with key length @n@ over @a@,
-- using gadget indicated by @gad@.
data PRFState t n gad rq = PRFState { params :: PRFParams   n gad rq
                                    , state' :: PRFState' t n gad rq }

data PRFState' t n gad rq where
  L :: BitStringMatrix 'Leaf rq
    -> PRFState' 'Leaf n gad rq
  I :: BitStringMatrix ('Intern l r) rq
    -> PRFState' l n gad rq     -- | left child
    -> PRFState' r n gad rq     -- | right child
    -> PRFState' ('Intern l r) n gad rq

-- | A 'BitString' together with a 'Matrix.T'
data BitStringMatrix t a
  = BSM { bitString :: BitString (SizeFBT t), matrix :: Matrix a }

root'  :: PRFState'             t n gad a -> BitStringMatrix t a
left'  :: PRFState' ('Intern l r) n gad a -> PRFState' l n gad a
right' :: PRFState' ('Intern l r) n gad a -> PRFState' r n gad a

root'  (L a)     = a
root'  (I a _ _) = a
left'  (I _ l _) = l
right' (I _ _ r) = r

root :: PRFState             t n gad a -> BitStringMatrix t a
root = root' . state'

-- | Compute PRF state for a given tree and input, which includes \(
-- \mathbf{A}_T(x) \) and all intermediate values (see Definition 2.1
-- of [BP14]).
updateState' :: forall gad rq t n . Decompose gad rq
  => SFBT t                     -- | singleton for the tree \( T \)
  -> PRFParams n gad rq
  -> Maybe (PRFState' t n gad rq)
  -> BitString (SizeFBT t)      -- | input \( x \)
  -> PRFState' t n gad rq
updateState' t p st x = case t of
  SLeaf       -> L $ BSM x $ if head x then a1 p else a0 p
  SIntern _ _  | fromMaybe False (((x ==) . bitString . root') <$> st)
                 -> fromJust st
  SIntern l r -> let (xl, xr) = splitS (sSizeFBT l) x
                     stl = updateState' l p (left'  <$> st) xl
                     str = updateState' r p (right' <$> st) xr
                     al   = matrix $ root' stl
                     ar   = matrix $ root' str
                     ar'  = reduce <$> decomposeMatrix @gad ar
                 in I (BSM x (al*ar')) stl str

updateState :: Decompose gad rq
  => SFBT t
  -> Either (PRFParams n gad rq) (PRFState t n gad rq)
  -> BitString (SizeFBT t)
  -> PRFState t n gad rq
updateState t e x =
  let p = either id params e
      st' = case e of           -- using fromRight gives weird error
        (Left  _)  -> Nothing
        (Right st) -> Just $ state' st
  in PRFState p $ updateState' t p st' x

-- | Compute \( \lfloor s \cdot \mathbf{A}_T \rceil_p \), where \(
-- \mathbf{A}_T(x) \) comes from the given state.
prfCore :: (Ring rq, Rescale rq rp)
  => PRFKey n rq -> PRFState t n gad rq -> Matrix rp
prfCore s st = rescale <$> (key s) * matrix (root st)

-- | "Fresh" PRF computation, with no precomputed 'PRFState'.
prf :: (Rescale rq rp, Decompose gad rq)
  => SFBT t                     -- | singleton for the tree \( T \)
  -> PRFParams n gad rq         -- | public parameters
  -> PRFKey n rq                -- | secret key \( s \)
  -> BitString (SizeFBT t)      -- | input \( x \)
  -> Matrix rp
prf = (fmap . fmap . fmap) fst . prfState

-- | "Fresh" PRF computation that also outputs the resulting 'PRFState'.
prfState :: (Rescale rq rp, Decompose gad rq)
  => SFBT t                     -- | singleton for the tree \( T \)
  -> PRFParams n gad rq         -- | public parameters
  -> PRFKey n rq                -- | secret key \( s \)
  -> BitString (SizeFBT t)      -- | input \( x \)
  -> (Matrix rp, PRFState t n gad rq)
prfState t p s x = let st = updateState t (Left p) x in (prfCore s st, st)

-- | Amortized PRF computation for a given secret key and input. The
-- output is in a monadic context that keeps a 'PRFState' state for
-- efficient amortization across calls.
prfAmortized ::
  (Rescale rq rp, Decompose gad rq,
   MonadState (Maybe (PRFState t n gad rq)) m)
  => SFBT t                     -- | singleton for the tree \( T \)
  -> PRFParams n gad rq         -- | public parameters
  -> PRFKey n rq                -- | secret key \( s \)
  -> BitString (SizeFBT t)      -- | input \( x \)
  -> m (Matrix rp)              -- | PRF output
prfAmortized t p s x = do
  fbt <- get
  let fbt' = updateState t (maybe (Left p) Right fbt) x
  put $ Just fbt'
  return $ prfCore s fbt'

-- | Run a PRF computation with some public parameters.
-- E.g.: @run top params (prf key x)@
run :: State (Maybe (PRFState t n gad rq)) a -> a
run = runIdentity . runT

-- | More general (monad transformer) version of 'run'.
runT :: (Monad m) => StateT (Maybe (PRFState t n gad rq)) m a -> m a
runT = flip evalStateT Nothing

-- | Canonical type-safe sized vector
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
                         num = 2 * thresh
                     in  \x -> let x' = x `mod` num
                               in if x' < thresh
                                  then False :- toEnum x'
                                  else True  :- toEnum (num - 1 - x')
  fromEnum = case (sing :: Sing n) of
               SO   -> \(Lone x) -> if x then 1 else 0
               SS m -> withSingI m $
                       let num :: Int = 2^(1 + sPosToInt m)
                       in \(x:-xs) -> if x
                                      then num - 1 - fromEnum xs
                                      else fromEnum xs

instance PosC n => Enumerable (Vector n Bool) where
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
split = splitS (sing :: Sing m)

-- | Alternative form of 'split'
splitS :: SPos m -> Vector (m `AddPos` n) a -> (Vector m a, Vector n a)
splitS m (h :- t) = case m of
  SO    -> (Lone h, t)
  SS m' -> let (b, e) = splitS m' t in (h :- b, e)
splitS _ (Lone _) = error "splitS: internal error; can't split a Lone"

-- | Create a 'Vector' full of given value
replicate :: forall n a . PosC n => a -> Vector n a
replicate = replicateS (sing :: Sing n)

-- | Alternative form of 'replicate'
replicateS :: SPos n -> a -> Vector n a
replicateS n a = case n of
  SO    -> Lone a
  SS n' -> a :- replicateS n' a

-- | Convert a list to a 'Vector', return 'Nothing' if lengths don't match
fromList :: forall n a . PosC n => [a] -> Maybe (Vector n a)
fromList = fromListS (sing :: Sing n)

fromListS :: SPos n -> [a] -> Maybe (Vector n a)
fromListS n xs = case n of
  SO    -> case xs of
             (x:[]) -> Just (Lone x)
             _      -> Nothing
  SS n' -> case xs of
             (x:rest) -> (:-) x <$> fromListS n' rest
             _        -> Nothing

sPosToInt :: SPos n -> Int
sPosToInt SO     = 1
sPosToInt (SS a) = 1 + sPosToInt a


{-|
Note: Making 'Vector' an instance of 'Additive.C'
Option 1: Two separate instances for Vector `O Bool and Vector (`S n) Bool
    Recursive instance requires recursive restraint
    Can't always match on these instances if we only know that n :: Pos
Option 2: One instance, using singletons and 'case' to distinguish
    Ugly syntax
    Didn't implement (functionality replaced by 'replicate')
-}
