{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Convenient interfaces for serialization with protocol buffers.

module Crypto.Lol.Types.Proto
(Protoable(..), msgPut, msgGet
,toProtoTuple, toProtoNestLeft, toProtoNestRight
,fromProtoTuple, fromProtoNestLeft, fromProtoNestRight
) where

import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Factored
import Crypto.Lol.Types

import Crypto.Proto.RLWE.Kq (Kq)
import Crypto.Proto.RLWE.KqProduct (KqProduct)
import Crypto.Proto.RLWE.Rq (Rq)
import Crypto.Proto.RLWE.RqProduct (RqProduct)

import Control.Monad.Except
import Data.ByteString.Lazy ()--  hiding (map)
import Data.Foldable (toList)
import Data.Sequence (fromList, (<|), (|>))

import Text.ProtocolBuffers        (messageGet, messagePut)
import Text.ProtocolBuffers.Header

-- | Conversion between Haskell types and their protocol buffer representations.
class Protoable a where
  -- | The protocol buffer type for @a@.
  type ProtoType a

  -- | Convert from a type to its protocol buffer representation.
  toProto :: a -> ProtoType a
  -- | Convert from a protocol buffer representation.
  fromProto :: MonadError String m => ProtoType a -> m a

instance (Protoable a) => Protoable [a] where
  type ProtoType [a] = Seq (ProtoType a)
  toProto = fromList . map toProto
  fromProto = mapM fromProto . toList

instance (Protoable a, Protoable b) => Protoable (a,b) where
  type ProtoType (a,b) = (ProtoType a, ProtoType b)
  toProto (a,b) = (toProto a, toProto b)
  fromProto (a,b) = do
    a' <- fromProto a
    b' <- fromProto b
    return (a',b')

-- | Serialize a Haskell type to its protocol buffer 'ByteString'.
msgPut :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a)
          => a -> ByteString
msgPut = messagePut . toProto

-- | Read a protocol buffer 'ByteString' to a Haskell type.
msgGet :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a)
          => ByteString -> Either String (a, ByteString)
msgGet bs = do
  (msg, bs') <- messageGet bs
  p <- fromProto msg
  return (p, bs')

type family ProdType a where
  ProdType (ZqBasic q i) = RqProduct
  ProdType (RRq q i) = KqProduct

type family SingleType a where
  SingleType (ZqBasic q i) = Rq
  SingleType (RRq q i) = Kq

toProtoTuple ::
  (Protoable (t m a), Protoable (t m b),
   ProtoType (t m a) ~ SingleType a,
   ProtoType (t m a) ~ ProtoType (t m b),
   Tensor t, TElt t (a,b), TElt t a, TElt t b, Fact m)
  => (Seq (SingleType a) -> ProdType a)
  -> t m (a,b)
  -> ProdType a
toProtoTuple box xs =
  let (as,bs) = unzipT xs
      as' = toProto as
      bs' = toProto bs
  in box $ fromList $ [as', bs']

fromProtoTuple ::
  (MonadError String mon,
   Protoable (t m a), Protoable (t m b),
   ProtoType (t m a) ~ SingleType a,
   ProtoType (t m a) ~ ProtoType (t m b),
   Tensor t, TElt t (a,b), TElt t a, TElt t b, Fact m)
  => (ProdType a -> Seq (SingleType a))
  -> ProdType a
  -> mon (t m (a,b))
fromProtoTuple unbox xs =
  let ys = toList $ unbox xs
  in case ys of
    [as,bs] -> do
      as' <- fromProto as
      bs' <- fromProto bs
      return $ zipWithT (,) as' bs'
    _ -> throwError $ "Expected list of length 2, received list of length " ++
           (show $ length ys)

toProtoNestLeft ::
  (Protoable (t m a), Protoable (t m b),
   ProtoType (t m a) ~ ProdType b,
   ProtoType (t m b) ~ SingleType b,
   Tensor t, TElt t (a,b), TElt t a, TElt t b, Fact m)
  => (Seq (SingleType b) -> ProdType b)
  -> (ProdType b -> Seq (SingleType b))
  -> t m (a,b)
  -> ProdType b
toProtoNestLeft box unbox xs =
    let (as,bs) = unzipT xs
        as' = unbox $ toProto as
        bs' = toProto bs
    in box $ as' |> bs'

fromProtoNestLeft ::
  (MonadError String mon,
   Protoable (t m a), Protoable (t m b),
   ProtoType (t m b) ~ SingleType b,
   ProtoType (t m a) ~ ProdType b,
   Tensor t, TElt t (a,b), TElt t a, TElt t b, Fact m)
  => (Seq (SingleType b) -> ProdType b)
  -> (ProdType b -> Seq (SingleType b))
  -> ProdType b
  -> mon (t m (a,b))
fromProtoNestLeft box unbox xs =
  let ys = toList $ unbox xs
  in if length ys >= 3
     then do
       as' <- fromProto $ box $ fromList $ init ys
       bs' <- fromProto $ last ys
       return $ zipWithT (,) as' bs'
      else throwError $
             "Expected list of length >= 3, received list of length " ++
             (show $ length ys)

toProtoNestRight ::
  (Protoable (t m a), Protoable (t m b),
   ProtoType (t m a) ~ SingleType a,
   ProtoType (t m b) ~ ProdType a,
   Tensor t, TElt t (a,b), TElt t a, TElt t b, Fact m)
  => (Seq (SingleType a) -> ProdType a)
  -> (ProdType a -> Seq (SingleType a))
  -> t m (a,b)
  -> ProdType a
toProtoNestRight box unbox xs =
    let (as,bs) = unzipT xs
        as' = toProto as
        bs' = unbox $ toProto bs
    in box $ as' <| bs'

fromProtoNestRight ::
  (MonadError String mon,
   Protoable (t m a), Protoable (t m b),
   ProtoType (t m a) ~ SingleType a,
   ProtoType (t m b) ~ ProdType a,
   Tensor t, TElt t (a,b), TElt t a, TElt t b, Fact m)
  => (Seq (SingleType a) -> ProdType a)
  -> (ProdType a -> Seq (SingleType a))
  -> ProdType a
  -> mon (t m (a,b))
fromProtoNestRight box unbox xs =
  let ys = toList $ unbox xs
  in if length ys >= 3
     then do
       as' <- fromProto $ head ys
       bs' <- fromProto $ box $ fromList $ tail ys
       return $ zipWithT (,) as' bs'
      else throwError $
             "Expected list of length >= 3, received list of length " ++
             (show $ length ys)