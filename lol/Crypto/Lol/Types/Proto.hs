{-# LANGUAGE FlexibleContexts, FlexibleInstances, PolyKinds, ScopedTypeVariables, TypeFamilies #-}
module Crypto.Lol.Types.Proto where

import Crypto.Lol.LatticePrelude (proxy, Proxy(..), lift, reduce, map)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.RealQ
import Crypto.Lol.Types.ZqBasic
import Crypto.Lol.Types.Proto.Coeffs
import Crypto.Lol.Types.Proto.Int64List
import Crypto.Lol.Types.Proto.DoubleList

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
  toProto = Zqs . (Int64List Nothing) . fromList
  fromProto (Zqs (Int64List Nothing xs)) = toList xs

instance (Reflects q Int64) => Protoable [ZqBasic q Int64] where
  type ProtoType [ZqBasic q Int64] = Coeffs
  toProto xs = Zqs $ 
    Int64List (Just $ fromIntegral (proxy value (Proxy::Proxy q) :: Int64)) $ fromList $ map lift xs
  fromProto (Zqs (Int64List (Just q') xs)) = 
    let q = proxy value (Proxy::Proxy q) :: Int64
    in if q == (fromIntegral q')
       then map reduce $ toList xs
       else error $ "Mismatched q value in Protoable instance for ZqBasic. Expected " ++ (show q) ++ ", got " ++ (show q') ++ "."

instance Protoable [Double] where
  type ProtoType [Double] = Coeffs
  toProto = Rqs . (DoubleList Nothing) . fromList
  fromProto (Rqs (DoubleList Nothing xs)) = toList xs

instance (Reflects q Int64) => Protoable [RealQ q Double Int64] where
  type ProtoType [RealQ q Double Int64] = Coeffs
  toProto xs = Rqs $ 
    DoubleList (Just $ fromIntegral (proxy value (Proxy::Proxy q) :: Int64)) $ fromList $ map lift xs
  fromProto (Rqs (DoubleList (Just q') xs)) = 
    let q = proxy value (Proxy::Proxy q) :: Int64
    in if q == (fromIntegral q')
       then map reduce $ toList xs
       else error $ "Mismatched q value in Protoable instance for RealQ. Expected " ++ (show q) ++ ", got " ++ (show q') ++ "."

msgPut :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a) => a -> ByteString
msgPut = messagePut . toProto

msgGet :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a) => ByteString -> Either String (a, ByteString)
msgGet bs = do
  (msg, bs') <- messageGet bs
  return (fromProto msg, bs')