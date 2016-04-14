


-- | Verify that the (scaled, squared) norm of the noise for each sample in the instance is below @mhat*n*v@.
checkInstance :: forall v t m zq . (CheckSample v t m zq)
  => v -> Cyc t m (LiftOf zq) -> [LWESample t m zq] -> Bool
checkInstance v sk samples = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
      eps = 1/(2^(40 :: Int))
      --d = (1/pi)*(1/2-(log eps)/(fromIntegral n))
      d = computeD n eps
      bound = (fromIntegral $ mhat*n)*v*d
  in all (checkSample bound sk) samples

type CheckSample v t m zq = 
  (CheckErr v t m zq, Ord v, Field v, Transcendental v, Show v)

-- | Verify that the (scaled, squared) norm of the noise for an LWE sample is below the provided bound.
checkSample :: forall v t m zq . (CheckSample v t m zq) 
  => v -> Cyc t m (LiftOf zq) -> LWESample t m zq -> Bool
checkSample bound sk pair@(LWESample a b) = (sampleError sk pair) < bound

type CheckErr v t m zq = 
  (Fact m, Ring v, Lift' zq, CElt t zq, CElt t (LiftOf zq), ToInteger (LiftOf zq))

-- | Given an instance and corresponding secret, outputs the (scaled, squared) norm of the error term.
sampleError :: forall v t m zq . (CheckErr v t m zq) 
  => Cyc t m (LiftOf zq) -> LWESample t m zq -> v
sampleError sk (LWESample a b) = 
  let e' = b-a*reduce sk
      e = liftCyc Dec e' :: Cyc t m (LiftOf zq)
      norm = gSqNorm e
  in fromIntegral norm

computeD :: (Field v, Ord v, Transcendental v) => Int -> v -> v
computeD n eps = go (1 / (2*pi))
  where go d = 
          let d' = (1/2 + (log $ 2 * pi * d)/2 - (log eps)/(fromIntegral n))/pi
          in if ((d'-d) < 0.0001)
             then d'
             else go d'