{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Convenient interfaces for serialization with protocol buffers.

module Crypto.Lol.Types.Proto
(Protoable(..), msgPut, msgGet
,toProtoProduct, fromProtoNestLeft, fromProtoNestRight
,uToString, uFromString
,readProtoType, parseProtoFile
,writeProtoType, writeProtoFile
,ProtoReadable
) where

import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Factored
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

toProtoProduct ::
  (Protoable (t m a), Protoable (t m b),
   ProtoType (t m a) ~ ProtoType (t m b),
   Tensor t, TElt t (a,b), TElt t a, TElt t b, Fact m)
  => (Seq c -> ProtoType (t m a))
  -> (ProtoType (t m a) -> Seq c)
  -> t m (a,b)
  -> ProtoType (t m a)
toProtoProduct box unbox xs =
  let (as,bs) = unzipT xs
      as' = unbox $ toProto as
      bs' = unbox $ toProto bs
  in box $ as' >< bs'

-- for tuples like ((a, b), c)
fromProtoNestLeft ::
  (MonadError String mon,
   Protoable (t m a), Protoable (t m b),
   ProtoType (t m a) ~ ProtoType (t m b),
   Tensor t, TElt t (a,b), TElt t a, TElt t b, Fact m)
  => (Seq c -> ProtoType (t m a))
  -> (ProtoType (t m a)-> Seq c)
  -> ProtoType (t m a)
  -> mon (t m (a,b))
fromProtoNestLeft box unbox xs = do
  let ys = unbox xs
  unless (length ys >= 2) $ throwError $
    "Expected list of length >= 2, received list of length " ++ show (length ys)
  let (as :> b) = viewr ys
  as' <- fromProto $ box as
  b' <- fromProto $ box $ singleton b
  return $ zipWithT (,) as' b'

-- for tuples like (a, (b, c))
fromProtoNestRight ::
  (MonadError String mon,
   Protoable (t m a), Protoable (t m b),
   ProtoType (t m a) ~ ProtoType (t m b),
   Tensor t, TElt t (a,b), TElt t a, TElt t b, Fact m)
  => (Seq c -> ProtoType (t m a))
  -> (ProtoType (t m a)-> Seq c)
  -> ProtoType (t m a)
  -> mon (t m (a,b))
fromProtoNestRight box unbox xs = do
  let ys = unbox xs
  unless (length ys >= 2) $ throwError $
    "Expected list of length >= 2, received list of length " ++ show (length ys)
  let (a :< bs) = viewl ys
  a' <- fromProto $ box $ singleton a
  bs' <- fromProto $ box bs
  return $ zipWithT (,) a' bs'

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

parseProtoFile :: (ProtoReadable a, MonadIO m, MonadError String m)
  => FilePath -> m a
parseProtoFile file = fromProto =<< readProtoType file

writeProtoFile :: (ProtoReadable a, MonadIO m) => FilePath -> a -> m ()
writeProtoFile file = liftIO . writeProtoType file . toProto

instance Protoable Fingerprint where
  type ProtoType Fingerprint = TypeRep
  toProto (Fingerprint a b) = TypeRep a b
  fromProto (TypeRep a b) = return $ Fingerprint a b