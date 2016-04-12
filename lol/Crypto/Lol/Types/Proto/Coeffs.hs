{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Lol.Types.Proto.Coeffs where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Lol.Types.Proto.Int64List as Lol (Int64List)
import qualified Crypto.Lol.Types.Proto.RealQList as Lol (RealQList)
import qualified Crypto.Lol.Types.Proto.ZqList as Lol (ZqList)

data Coeffs = Zs{zs :: (Lol.Int64List)}
            | Rqs{rqs :: (Lol.RealQList)}
            | Zqs{zqs :: (Lol.ZqList)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
get'zs x
 = case x of
     Zs zs -> Prelude'.Just zs
     _ -> Prelude'.Nothing
get'rqs x
 = case x of
     Rqs rqs -> Prelude'.Just rqs
     _ -> Prelude'.Nothing
get'zqs x
 = case x of
     Zqs zqs -> Prelude'.Just zqs
     _ -> Prelude'.Nothing

instance P'.Default Coeffs where
  defaultValue = Zs P'.defaultValue

instance P'.Mergeable Coeffs