{-# LANGUAGE FlexibleContexts, FlexibleInstances, PolyKinds,
             ScopedTypeVariables, TypeFamilies #-}

module Crypto.Lol.Types.Proto where

import Control.Monad.Except
import Data.ByteString.Lazy hiding (map)
import Data.Foldable (toList)
import Data.Sequence (fromList)

import Text.ProtocolBuffers        (messageGet, messagePut)
import Text.ProtocolBuffers.Header

class Protoable a where
  type ProtoType a

  toProto :: a -> ProtoType a
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

msgPut :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a)
          => a -> ByteString
msgPut = messagePut . toProto

msgGet :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a)
          => ByteString -> Either String (a, ByteString)
msgGet bs = do
  (msg, bs') <- messageGet bs
  p <- fromProto msg
  return (p, bs')
