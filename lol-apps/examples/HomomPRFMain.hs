{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
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

import Crypto.Lol hiding (lift)
import Crypto.Lol.Applications.HomomPRF
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Cyclotomic.Tensor.CPP as CPP
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Random
import Crypto.Random.DRBG

import Data.Promotion.Prelude.List
import Data.Time.Clock
import Data.Typeable
import System.FilePath     ((</>), pathSeparator)
import System.IO

import HomomPRFParams

type T = CPP.CT
type Z = Int64

protoDir :: Int -> String -> String
protoDir p = (((pathSeparator : "home") </> "eric" </> "Desktop" </> "Lol" </> ("p" ++ show p ++ "-")) ++)

thintPath, rhintPath, tskPath, rskPath :: Int -> String
thintPath p = protoDir p "tunnel.hint"
rhintPath p = protoDir p "round.hint"
-- | The key used as the input to tunneling; also used for encryption
tskPath   p = protoDir p "encKey.secret"
-- | The output key of tunneling, used for rounding; also for decryption
rskPath   p = protoDir p "decKey.secret"

-- This function serves the dual purpose of specifying the random generator
-- and keeping all of the code in the IO monad, which helps write clean code below
-- No sequencing occurs between separate calls to this function, but it would be hard
-- to get timing any other way.
evalHashDRBG :: RandT (CryptoRand HashDRBG) IO a -> IO a
evalHashDRBG = evalCryptoRandIO

main :: IO ()
main = do
  putStrLn $ "Starting homomorphic PRF evaluation with tensor " ++ show (typeRep (Proxy::Proxy T))
  (hints, decKey, family, ct) <- time "Generating random inputs..." =<< evalHashDRBG (do
    (h, encKey, d) <- readOrGenHints
    f :: PRFFamily PRFGad _ _ <- randomFamily 10 -- works on 10-bit input
    s <- getRandom
    ct <- encrypt encKey s
    return (h,d,f,ct))
  st <- time "Initializing PRF state..." $ prfState family Nothing --initialize with input 0
  encprf <- time "Evaluating PRF..." $ flip runReader hints $ flip evalStateT st $ homomPRFM ct 0
  _ <- time "Decrypting PRF output..." $ decrypt decKey encprf
  return ()

readHints :: forall mon t rngs z zp zq zqs gad r' s' .
  (MonadIO mon, MonadError String mon, Mod zp,
   ProtoReadable (HTunnelHints gad t rngs zp (ZqUp zq zqs)),
   ProtoReadable (RoundHints t (Fst (Last rngs)) (Snd (Last rngs)) z zp (ZqDown zq zqs) zqs gad),
   ProtoReadable (SK (Cyc t r' z)), ProtoReadable (SK (Cyc t s' z)))
  => mon (EvalHints t rngs z zp zq zqs gad, SK (Cyc t r' z), SK (Cyc t s' z))
readHints = do
  let p = fromIntegral $ proxy modulus (Proxy::Proxy zp)
  tHints <- parseProtoFile $ thintPath p
  rHints <- parseProtoFile $ rhintPath p
  tsk    <- parseProtoFile $ tskPath p
  rsk    <- parseProtoFile $ rskPath p
  return (Hints tHints rHints, tsk, rsk)

readOrGenHints :: (MonadIO mon, MonadRandom mon, Head RngList ~ '(r,r'), Last RngList ~ '(s,s'))
  => mon (EvalHints T RngList Z ZP ZQ ZQSeq KSGad, SK (Cyc T r' Z), SK (Cyc T s' Z))
readOrGenHints = do
  res <- runExceptT readHints
  case res of
    (Left st) -> do
      liftIO $ putStrLn $ "Could not read precomputed data. Error was: " ++ st
      (hints, sk, skout) <- do
        let v = 1.0 :: Double
        sk <- genSK v
        (tHints, skout) <- tunnelHints sk
        rHints <- roundHints skout
        let hints = Hints tHints rHints
        return (hints, sk, skout)
      liftIO $ putStrLn "Writing hints to disk..."
      writeHints hints sk skout
      return (hints, sk, skout)
    (Right a) -> do
      liftIO $ putStrLn "Precomputed hints found."
      return a

writeHints :: forall mon t rngs z zp zq zqs gad r' s' .
  (MonadIO mon, Mod zp,
   ProtoReadable (HTunnelHints gad t rngs zp (ZqUp zq zqs)),
   ProtoReadable (RoundHints t (Fst (Last rngs)) (Snd (Last rngs)) z zp (ZqDown zq zqs) zqs gad),
   ProtoReadable (SK (Cyc t r' z)), ProtoReadable (SK (Cyc t s' z)))
  => EvalHints t rngs z zp zq zqs gad
  -> SK (Cyc t r' z)
  -> SK (Cyc t s' z)
  -> mon ()
writeHints (Hints tHints rHints) encKey decKey = do
  let p = fromIntegral $ proxy modulus (Proxy::Proxy zp)
  writeProtoFile (thintPath p) tHints
  writeProtoFile (rhintPath p) rHints
  writeProtoFile (tskPath p) encKey
  writeProtoFile (rskPath p) decKey


-- timing functionality
time :: (NFData a, MonadIO m) => String -> a -> m a
time s m = liftIO $ do
  putStr' s
  wallStart <- getCurrentTime
  m `deepseq` printTimes Nothing wallStart 1
  return m

-- flushes the print buffer
putStr' :: String -> IO ()
putStr' str = do
  putStrLn str
  hFlush stdout

--rounds to precision, for pretty printing
toPrecision :: (Field a, RealRing a) => Integer -> a -> a
toPrecision n x = fromInteger $ round $ x * (10^n) / (10.0^n)

printTimes :: Maybe Bool -> UTCTime -> Int -> IO ()
printTimes _ wallStart iters = do
    wallEnd <- getCurrentTime
    let wallTime = realToFrac $ diffUTCTime wallEnd wallStart :: Double
    putStr $ "Wall time: " ++ show (toPrecision 2 wallTime) ++ "s"
    if iters == 1
    then putStr "\n\n"
    else putStr $ "\tAverage wall time: " ++ show (toPrecision 2 $ wallTime / fromIntegral iters) ++ "s\n\n"
