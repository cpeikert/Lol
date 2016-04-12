{-# LANGUAGE FlexibleContexts, FlexibleInstances, PolyKinds, ScopedTypeVariables, TypeFamilies #-}
module Crypto.Lol.Types.Proto where

import Crypto.Lol.LatticePrelude (proxy, Proxy(..), lift, reduce, map)
import Crypto.Lol.Types.Proto.Coeffs
import Crypto.Lol.Types.Proto.Int64List

import Data.ByteString.Lazy hiding (map)
import Data.Foldable (toList)
import Data.Sequence as S (fromList)

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers (messageGet, messagePut)

class Protoable a where
  type ProtoType a

  toProto :: a -> ProtoType a
  fromProto :: ProtoType a -> a

instance Protoable [Int64] where
  type ProtoType [Int64] = Coeffs
  toProto = Zs . Int64List . fromList
  fromProto (Zs (Int64List xs)) = toList xs

msgPut :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a) => a -> ByteString
msgPut = messagePut . toProto

msgGet :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a) => ByteString -> Either String (a, ByteString)
msgGet bs = do
  (msg, bs') <- messageGet bs
  return (fromProto msg, bs')