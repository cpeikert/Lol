{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module HomomPRFMain where

import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import Crypto.Lol
import Crypto.Lol.Applications.HomomPRF
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Cyclotomic.Tensor.CPP as CPP
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto

import Data.Promotion.Prelude.List
import System.FilePath     ((</>))

import HomomPRFParams

type T = CPP.CT
type Z = Int64

protoDir :: Int -> String -> String
protoDir p = (("~" </> "Desktop" </> "Lol" </> ("p" ++ (show p))) </>)

thintPath p = protoDir p "tunnel.hint"
rhintPath p = protoDir p "round.hint"
tskPath   p = protoDir p "encKey.secret"
rskPath   p = protoDir p "decKey.secret"


main :: IO ()
main = do
  (hints, encKey, decKey) <- readOrGenHints
  family :: PRFFamily PRFGad _ _ <- randomFamily 10 -- works on 10-bit input
  s <- getRandom
  let st = prfState family Nothing --initialize with input 0
  ct <- encrypt encKey s
  let prf = homomPRFM ct
      xs = grayCode 3
      encprfs = flip runReader hints $ flip evalStateT st $ mapM prf xs
      decprfs = decrypt decKey <$> encprfs
  decprfs `deepseq` return ()

readHints :: forall mon t rngs z zp zq zqs gad r' s' .
  (MonadIO mon, MonadError String mon, Reflects zp Int,
   ProtoReadable (HTunnelHints gad t rngs zp (ZqUp zq zqs)),
   ProtoReadable (RoundHints t (Fst (Last rngs)) (Snd (Last rngs)) z zp (ZqDown zq zqs) zqs gad),
   ProtoReadable (SK (Cyc t r' z)), ProtoReadable (SK (Cyc t s' z)))
  => mon (EvalHints t rngs z zp zq zqs gad, SK (Cyc t r' z), SK (Cyc t s' z))
readHints = do
  let p = proxy value (Proxy::Proxy zp) :: Int
  tHints <- parseProtoFile $ thintPath p
  rHints <- parseProtoFile $ rhintPath p
  tsk    <- parseProtoFile $ tskPath p
  rsk    <- parseProtoFile $ rskPath p
  return (Hints tHints rHints, tsk, rsk)

readOrGenHints :: (MonadIO mon, Head RngList ~ '(r,r'), Last RngList ~ '(s,s'))
  => mon (EvalHints T RngList Z ZP ZQ ZQSeq KSGad, SK (Cyc T r' Z), SK (Cyc T s' Z))
readOrGenHints = do
  res <- runExceptT readHints
  case res of
    (Left st) -> liftIO $ do
      putStrLn $ "Could not read precomputed data. Error was: " ++ st ++
        "\nGenerating hints..."
      let v = 1.0 :: Double
      sk <- genSK v
      (tHints, skout) <- tunnelHints sk
      rHints <- roundHints skout
      let hints = Hints tHints rHints
      return (hints, sk, skout)
    (Right a) -> return a
