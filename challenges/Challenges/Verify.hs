{-# LANGUAGE FlexibleContexts, GADTs, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

module Challenges.Verify where

import Challenges.LWE

import Crypto.Lol

challengePath :: FilePath
challengePath = "challenge-files"

secretPath :: FilePath
secretPath = "secret-files"

topSecretPath :: FilePath
topSecretPath = "top-secret-files"

revealPath :: FilePath
revealPath = "reveal-files"

{-
verifyChallenge :: FilePath -> IO Bool
verifyChallenge challName = do

  -- read secret file
  secrets <- msgGet' <$> BS.readFile $ secretPath ++ "/" ++ challName
  -- read challenge file
  chall <- msgGet' <$> BS.readFile $ challengePath ++ "/" ++ challName

  numInstances = 
  -- check that there are e
-}


checkChallenge :: (Absolute (LiftOf zp), Ord (LiftOf zp), Fact m,
   Lift' zp, CElt t zp, CElt t (LiftOf zp), Ord v, 
   ToInteger (LiftOf zp), Ring v)
  => SecretLWEChallenge v t m zp -> Bool
checkChallenge (SLWEChallenge v insts) = all (checkInstance v) insts

checkInstance :: forall v t m zp . 
  (Absolute (LiftOf zp), Ord (LiftOf zp), Fact m, 
   Lift' zp, CElt t zp, CElt t (LiftOf zp), Ord v, 
   ToInteger (LiftOf zp), Ring v)
  => v -> SecretLWEInstance t m zp -> Bool
checkInstance v (SLWEInstance sk pairs) = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
      bound = (fromIntegral $ mhat*n)*v
  in all ((< bound) . sampleError sk) pairs

sampleError :: forall v t m zp . 
  (Absolute (LiftOf zp), Ord (LiftOf zp), Fact m, Ring v,
   Lift' zp, CElt t zp, CElt t (LiftOf zp), ToInteger (LiftOf zp)) 
  => Cyc t m (LiftOf zp) -> LWESample t m zp -> v
sampleError sk (LWESample a b) = 
  let e' = b-a*reduce sk :: Cyc t m zp
      e = liftCyc Dec e' :: Cyc t m (LiftOf zp)
      norm = gSqNorm e
  in fromIntegral norm