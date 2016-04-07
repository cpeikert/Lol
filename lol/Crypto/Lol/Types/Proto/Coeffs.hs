{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Lol.Types.Proto.Coeffs where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Lol.Types.Proto.DoubleList as Lol (DoubleList)
import qualified Crypto.Lol.Types.Proto.Int64List as Lol (Int64List)

data Coeffs = Zqs{zqs :: (Lol.Int64List)}
            | Rqs{rqs :: (Lol.DoubleList)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
get'zqs x
 = case x of
     Zqs zqs -> Prelude'.Just zqs
     _ -> Prelude'.Nothing
get'rqs x
 = case x of
     Rqs rqs -> Prelude'.Just rqs
     _ -> Prelude'.Nothing

instance P'.Default Coeffs where
  defaultValue = Zqs P'.defaultValue

instance P'.Mergeable Coeffs