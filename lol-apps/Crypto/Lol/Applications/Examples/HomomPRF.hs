{-|
Module      : Crypto.Lol.Applications.Examples.HomomPRF
Description : Example, test, and macro-benchmark for homomorphic evaluation of a PRF.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example, test, and macro-benchmark for homomorphic evaluation of a PRF.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Examples.HomomPRF (homomPRFMain) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import Crypto.Lol hiding (lift)
import Crypto.Lol.Applications.HomomPRF
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Random
import Crypto.Random.DRBG

import Data.Promotion.Prelude
import Data.Time.Clock
import Data.Typeable
import MathObj.Matrix  (columns)
import Options
import System.FilePath ((</>))
import System.IO
import Text.Printf

import Crypto.Lol.Applications.Examples.HomomPRFParams

type Z = Int64

lfuncPath, thintPath, rhintPath, tskPath, rskPath :: String -> Int -> String
thintPath root p = root </> "p" ++ show p ++ "-tunnel.hint"
rhintPath root p = root </> "p" ++ show p ++ "-round.hint"
lfuncPath root p = root </> "p" ++ show p ++ "-unnelfuncs.lfuns"
-- | The key used as the input to tunneling; also used for encryption
tskPath   root p = root </> "p" ++ show p ++ "-encKey.secret"
-- | The output key of tunneling, used for rounding; also for decryption
rskPath   root p = root </> "p" ++ show p ++ "-decKey.secret"

data MainOpts =
  MainOpts
  { optDataDir :: FilePath -- ^ location of precomputed data
  }

instance Options MainOpts where
  defineOptions = MainOpts <$>
    defineOption optionType_string (\o ->
      o {optionLongFlags = ["path"],
         optionShortFlags = ['p'],
         optionDefault = ".",
         optionDescription = "Path to precomputed data. If no data is found, \
           \it will be generated and saved to this location."})

-- | Driver for the homomorphic PRF evaluation.
--
-- R' - ... - S'
--
-- |  -  |  - |
--
-- R  - ... - S
--
-- PRF evaluation usually goes from R' to R. Homomorphic evaluation
-- goes from R' to S, via S'. To test, we run the computation in the clear
-- from R' to R, then tunnel in the clear to S.
--
-- The executable takes a path (specified with --path or -p, default "."), from
-- which it tries to read preomputed data. If the data is not found, it generates
-- the data and saves it to the path.
homomPRFMain :: forall t . (_) => Proxy t -> IO ()
homomPRFMain = runCommand . homomPRFMain'

homomPRFMain' :: forall t . (_) => Proxy t -> MainOpts -> [String] -> IO ()
homomPRFMain' pt MainOpts{..} _ = do
  putStrLn $ "Starting homomorphic PRF evaluation with tensor " ++ show (typeRep pt)
  hints' <- runExceptT $ readHints optDataDir
  (lfuns, hints :: EvalHints t RngList Z ZP ZQ ZQSeq KSGad, encKey, decKey) <- case hints' of
    (Right a) -> do
      putStrLn "Using precomputed hints."
      return a
    (Left st) -> do
      putStrLn $ "Could not read precomputed hints: " ++ st
      gen :: CryptoRand HashDRBG <- liftIO newGenIO -- uses system entropy
      (lfuns, hints, encKey, decKey) <- time "Generating hints..." $ flip evalRand gen $ do
        let v = 1.0 :: Double
        encKey <- genSK v
        (tHints, decKey) <- tunnelInfoChain encKey
        let lfuns = ptTunnelFuncs
        rHints <- roundHints decKey
        let hints = Hints tHints rHints
        return (lfuns, hints, encKey, decKey)
      writeHints optDataDir lfuns hints encKey decKey
      return (lfuns, hints, encKey, decKey)
  gen :: CryptoRand HashDRBG <- liftIO newGenIO -- uses system entropy
  (family, s, ct) <- time "Generating random inputs..." $ flip evalRand gen $ do
    family :: PRFFamily PRFGad _ _ <- randomFamily 10 -- works on 10-bit input
    s <- getRandom
    ct <- encrypt encKey s
    return (family, s, ct)
  st <- time "Initializing PRF state..." $ prfState family Nothing --initialize with input 0
  encprf <- time "Evaluating PRF..." $ flip runReader hints $ flip evalStateT st $ homomPRFM ct 0
  hprf <- time "Decrypting PRF output..." $ decrypt decKey encprf

  -- test
  clearPRF <- time "In-the-clear PRF..." $ head $ head $ columns $ ringPRF s 0 st -- homomPRF only computes first elt
  clearPRF' <- time "In-the-clear tunnel..." $ ptTunnel lfuns clearPRF
  if clearPRF' == hprf
  then putStrLn "PASS: Homomorphic output matches in-the-clear."
  else putStrLn "TEST FAILED"

readHints :: forall mon t rngs z e zp zq zqs gad r' s' .
  (MonadIO mon, MonadError String mon, Mod zp, UnPP (CharOf zp) ~ '(Prime2, e),
   ProtoReadable (TunnelFuncs t (PTRings rngs) (TwoOf zp)),
   ProtoReadable (TunnelInfoChain gad t rngs zp (ZqUp zq zqs)),
   ProtoReadable (RoundHints t (Fst (Last rngs)) (Snd (Last rngs)) z e zp (ZqDown zq zqs) zqs gad),
   ProtoReadable (SK (Cyc t r' z)), ProtoReadable (SK (Cyc t s' z)))
  => String -> mon (TunnelFuncs t (PTRings rngs) (TwoOf zp),
          EvalHints t rngs z zp zq zqs gad,
          SK (Cyc t r' z), SK (Cyc t s' z))
readHints root = do
  let p = fromIntegral $ proxy modulus (Proxy::Proxy zp)
  lfuns  <- parseProtoFile $ lfuncPath root p
  tHints <- parseProtoFile $ thintPath root p
  rHints <- parseProtoFile $ rhintPath root p
  tsk    <- parseProtoFile $ tskPath   root p
  rsk    <- parseProtoFile $ rskPath   root p
  return (lfuns, Hints tHints rHints, tsk, rsk)

writeHints :: forall mon t rngs z e zp zq zqs gad r' s' .
  (MonadIO mon, Mod zp, UnPP (CharOf zp) ~ '(Prime2, e),
   ProtoReadable (TunnelFuncs t (PTRings rngs) (TwoOf zp)),
   ProtoReadable (TunnelInfoChain gad t rngs zp (ZqUp zq zqs)),
   ProtoReadable (RoundHints t (Fst (Last rngs)) (Snd (Last rngs)) z e zp (ZqDown zq zqs) zqs gad),
   ProtoReadable (SK (Cyc t r' z)), ProtoReadable (SK (Cyc t s' z)))
  => String
  -> TunnelFuncs t (PTRings rngs) (TwoOf zp)
  -> EvalHints t rngs z zp zq zqs gad
  -> SK (Cyc t r' z)
  -> SK (Cyc t s' z)
  -> mon ()
writeHints root lfuns (Hints tHints rHints) encKey decKey = do
  let p = fromIntegral $ proxy modulus (Proxy::Proxy zp)
  writeProtoFile (lfuncPath root p) lfuns
  writeProtoFile (thintPath root p) tHints
  writeProtoFile (rhintPath root p) rHints
  writeProtoFile (tskPath   root p) encKey
  writeProtoFile (rskPath   root p) decKey

-- timing functionality
time :: (NFData a) => String -> a -> IO a
time s m = do
  putStr' s
  wallStart <- getCurrentTime
  m `deepseq` printTimes wallStart 1
  return m

-- flushes the print buffer
putStr' :: String -> IO ()
putStr' str = do
  putStr str
  hFlush stdout

printTimes :: UTCTime -> Int -> IO ()
printTimes wallStart iters = do
    wallEnd <- getCurrentTime
    let wallTime = realToFrac $ diffUTCTime wallEnd wallStart :: Double
    printf "Wall time: %0.3fs" wallTime
    if iters == 1
    then putStr "\n\n"
    else printf "\tAverage wall time: %0.3fs\n\n" $ wallTime / fromIntegral iters
