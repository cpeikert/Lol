{-# LANGUAGE FlexibleContexts, FlexibleInstances, PolyKinds,
             ScopedTypeVariables, TypeFamilies #-}

module Crypto.Lol.Types.Proto where

import Data.ByteString.Lazy hiding (map)

import Text.ProtocolBuffers        (messageGet, messagePut)
import Text.ProtocolBuffers.Header

class Protoable a where
  type ProtoType a

  toProto :: a -> ProtoType a
  fromProto :: ProtoType a -> a

msgPut :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a)
          => a -> ByteString
msgPut = messagePut . toProto

msgGet :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a)
          => ByteString -> Either String (a, ByteString)
msgGet bs = do
  (msg, bs') <- messageGet bs
  return (fromProto msg, bs')
