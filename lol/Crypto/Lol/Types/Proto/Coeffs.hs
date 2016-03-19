{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Crypto.Lol.Types.Proto.Coeffs where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Crypto.Lol.Types.Proto.ZList as Lol (ZList)
import qualified Crypto.Lol.Types.Proto.ZqList as Lol (ZqList)

data Coeffs = Zs{zs :: (Lol.ZList)}
            | Zqs{zqs :: (Lol.ZqList)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
get'zs x
 = case x of
     Zs zs -> Prelude'.Just zs
     _ -> Prelude'.Nothing
get'zqs x
 = case x of
     Zqs zqs -> Prelude'.Just zqs
     _ -> Prelude'.Nothing

instance P'.Default Coeffs where
  defaultValue = Zs P'.defaultValue

instance P'.Mergeable Coeffs