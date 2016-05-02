{-# LANGUAGE FlexibleContexts, FlexibleInstances, PolyKinds,
             ScopedTypeVariables, TypeFamilies #-}

-- | Convenient interfaces for serialization with protocol buffers.

module Crypto.Lol.Types.Proto where

import Control.Monad.Except
import Data.ByteString.Lazy hiding (map)
import Data.Foldable (toList)
import Data.Sequence (fromList)

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

-- | list instance
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
