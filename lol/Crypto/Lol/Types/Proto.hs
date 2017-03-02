{-|
Module      : Crypto.Lol.Types.Proto
Description : Convenient interfaces for serialization with protocol buffers.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Convenient interfaces for serialization with protocol buffers.
-}

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Crypto.Lol.Types.Proto
(Protoable(..), msgPut, msgGet
,uToString, uFromString
,readProtoType, parseProtoFile
,writeProtoType, writeProtoFile
,ProtoReadable
) where

import Crypto.Proto.Lol.TypeRep (TypeRep(TypeRep))

import Control.Monad.Except
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (toList)
import Data.Sequence
import GHC.Fingerprint

import Prelude hiding (length)
import System.Directory

import Text.ProtocolBuffers        (messageGet, messagePut)
import Text.ProtocolBuffers.Basic  (uToString, uFromString)
import Text.ProtocolBuffers.Header

-- | Constraint synonym for end-to-end reading/parsing/writing of 'Protoable' types.
type ProtoReadable a = (Protoable a, Wire (ProtoType a), ReflectDescriptor (ProtoType a))

-- | Conversion between Haskell types and their protocol buffer representations.
class Protoable a where

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

-- | Read a serialized protobuffer from a file.
readProtoType :: (ReflectDescriptor a, Wire a, MonadIO m, MonadError String m)
                 => FilePath -> m a
readProtoType file = do
  fileExists <- liftIO $ doesFileExist file
  unless fileExists $ throwError $
    "Error reading " ++ file ++ ": file does not exist."
  bs <- liftIO $ BS.readFile file
  case messageGet bs of
    (Left str) -> throwError $
      "Error when reading from protocol buffer. Got string " ++ str
    (Right (a,bs')) -> do
      unless (BS.null bs') $ throwError
        "Error when reading from protocol buffer. There were leftover bits!"
      return a

-- | Writes any auto-gen'd proto object to path/filename.
writeProtoType :: (ReflectDescriptor a, Wire a) => FilePath -> a -> IO ()
writeProtoType fileName = BS.writeFile fileName . messagePut

-- | Read a protocol buffer stream at the given path and convert it to typed
-- Haskell data.
parseProtoFile :: (ProtoReadable a, MonadIO m, MonadError String m)
  => FilePath -> m a
parseProtoFile file = fromProto =<< readProtoType file

-- | Write a protocol buffer stream for Haskell data to the given path.
writeProtoFile :: (ProtoReadable a, MonadIO m) => FilePath -> a -> m ()
writeProtoFile file = liftIO . writeProtoType file . toProto

instance Protoable Fingerprint where
  type ProtoType Fingerprint = TypeRep
  toProto (Fingerprint a b) = TypeRep a b
  fromProto (TypeRep a b) = return $ Fingerprint a b