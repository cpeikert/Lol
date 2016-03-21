{-# LANGUAGE ConstraintKinds, FlexibleContexts, GADTs, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

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


checkChallenge :: (CheckSample v t m zp) => SecretLWEChallenge v t m zp -> Bool
checkChallenge (SLWEChallenge v insts) = all (checkInstance v) insts

checkInstance :: forall v t m zp . (CheckSample v t m zp)
  => v -> SecretLWEInstance t m zp -> Bool
checkInstance v (SLWEInstance sk pairs) = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
      bound = (fromIntegral $ mhat*n)*v
  in all (checkSample bound sk) pairs

type CheckSample v t m zp = 
  (Show (Cyc t m (LiftOf zp)), Show (Cyc t m zp), CheckErr v t m zp, Ord v,
   Field v, Show v, Show (LiftOf zp))

checkSample :: (CheckSample v t m zp) 
  => v -> Cyc t m (LiftOf zp) -> LWESample t m zp -> Bool
checkSample bound sk pair@(LWESample a b) = 
  if (sampleError sk pair) < bound
  then True
  else error $ "\n" ++ 
    "sk " ++ (show sk) ++ "\n" ++
    "a  " ++ (show a) ++ "\n" ++ 
    "b  " ++ (show b) ++ "\n" ++ 
    "e  " ++ (show $ b-a*reduce sk) ++ "\n" ++
    "e' " ++ (show $ liftCyc Dec $ b-a*reduce sk) ++ "\n" ++
    "norm  " ++ (show $ gSqNorm $ liftCyc Dec $ b-a*reduce sk) ++ "\n" ++
    "bnd   " ++ (show bound) ++ "\n" ++
    "ratio " ++ (show $ bound / (fromIntegral $ gSqNorm $ liftCyc Dec $ b-a*reduce sk))

type CheckErr v t m zp = 
  (Fact m, Ring v, Lift' zp, CElt t zp, CElt t (LiftOf zp), ToInteger (LiftOf zp))

sampleError :: forall v t m zp . (CheckErr v t m zp) 
  => Cyc t m (LiftOf zp) -> LWESample t m zp -> v
sampleError sk (LWESample a b) = 
  let e' = b-a*reduce sk
      e = liftCyc Dec e' :: Cyc t m (LiftOf zp)
      norm = gSqNorm e
  in fromIntegral norm