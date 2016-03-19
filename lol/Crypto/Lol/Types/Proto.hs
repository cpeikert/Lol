{-# LANGUAGE FlexibleContexts, FlexibleInstances, PolyKinds, ScopedTypeVariables, TypeFamilies #-}
module Crypto.Lol.Types.Proto where

import Crypto.Lol.LatticePrelude (proxy, Proxy(..), lift, reduce, map)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.ZqBasic
import Crypto.Lol.Types.Proto.Coeffs
import Crypto.Lol.Types.Proto.ZList
import Crypto.Lol.Types.Proto.ZqList

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
  toProto = Zs . ZList . fromList
  fromProto (Zs (ZList xs)) = toList xs

instance (Reflects q Int64) => Protoable [ZqBasic q Int64] where
  type ProtoType [ZqBasic q Int64] = Coeffs
  toProto xs =
    let xs' = toProto $ map lift xs
    in case xs' of
      Zs zs -> Zqs $ ZqList (fromIntegral (proxy value (Proxy::Proxy q) :: Int64)) zs
      Zqs _ -> error "internal error: toProto of [Int64] should be Coeffs.Zs"
  fromProto (Zqs (ZqList q' xs)) = 
    let xs' = fromProto (Zs xs) :: [Int64]
        q = proxy value (Proxy::Proxy q) :: Int64
    in if q == (fromIntegral q')
       then map reduce xs'
       else error $ "Mismatched q value in Protoable instance for ZqBasic. Expected " ++ (show q) ++ ", got " ++ (show q') ++ "."

msgPut :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a) => a -> ByteString
msgPut = messagePut . toProto

msgGet :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a) => ByteString -> Either String (a, ByteString)
msgGet bs = do
  (msg, bs') <- messageGet bs
  return (fromProto msg, bs')