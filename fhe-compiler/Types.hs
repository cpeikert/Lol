{-|
Module      : Types
Description : Concrete types for the homomorphic operations.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Currently unmaintained.
-}

{-# LANGUAGE KindSignatures, PolyKinds, DataKinds, FlexibleInstances, RankNTypes,
             TypeOperators, ConstraintKinds, FlexibleContexts, ScopedTypeVariables,
             MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RebindableSyntax #-}

module Types (
  H0, H1, H2, H3, H4, H5        -- plaintext rings
, H0', H1', H2', H3', H4', H5'  -- ciphertext rings
, ZP2, ZP4, ZP8                 -- plaintext moduli
, ZQ1, ZQ2, ZQ3, ZQ4, ZQ5, ZQ6  -- ciphertext moduli

, ZPDiv2, ZQUp, ZQDown, NextListElt, PrevListElt, RngList, MyGad
, time) where

import Algebra.Ring

import Control.Monad
--import Control.Monad.Random

--import Crypto.Lol.Reflects
import Crypto.Lol hiding (primes, (^))
import Crypto.Lol.Types

--import Data.List (foldl1')
--import Data.Type.Natural
--import Data.Maybe hiding (fromJust)

--import Math.NumberTheory.Primes.Testing (isPrime)
--import Math.NumberTheory.Primes.Factorisation (factorise)

import Data.Time.Clock
import System.IO
import Control.DeepSeq
import Data.Dynamic
import Data.Promotion.Prelude.List

type family MyGad

type instance MyGad = TrivGad

type ZPDiv2 zp = NextListElt zp '[ZP8, ZP4, ZP2]
type ZQSeq = '[ZQ6, ZQ5, ZQ4, ZQ3, ZQ2, ZQ1]
type ZQUp zq = PrevListElt zq ZQSeq
type ZQDown zq = NextListElt zq ZQSeq

type family NextListElt (x :: k) (xs :: [k]) :: k where
  NextListElt x (x ': y ': ys) = y
  NextListElt x (y ': ys) = NextListElt x ys

type PrevListElt x xs = NextListElt x (Reverse xs)

type RngList = '[ '(H0,H0'), '(H1,H1'), '(H2,H2'), '(H3,H3'), '(H4,H4'), '(H5,H5') ]

type H0 = $(fType $ 2^7)
type H1 = $(fType $ 2^6 * 7)
type H2 = $(fType $ 2^5 * 7 * 13)
type H3 = $(fType $ 2^3 * 5 * 7 * 13)
type H4 = $(fType $ 2^2 * 3 * 5 * 7 * 13)
type H5 = $(fType $ 3^2 * 5 * 7 * 13)
type H0' = $(fType $ 2^7 * 7 * 13)
type H1' = $(fType $ 2^6 * 7 * 13)
type H2' = $(fType $ 2^5 * 7 * 13)
type H3' = $(fType $ 2^3 * 5 * 7 * 13)
type H4' = $(fType $ 2^2 * 3 * 5 * 7 * 13)
type H5' = $(fType $ 3^2 * 5 * 7 * 13)

-- three 24-bit moduli, enough to handle rounding for p=32 (depth-4 circuit at ~17 bits per mul)
type ZQ1 = Zq 18869761
type ZQ2 = (Zq 19393921, ZQ1)
type ZQ3 = (Zq 19918081, ZQ2)
-- a 31-bit modulus, for rounding off after the last four hops
type ZQ4 = (Zq 2149056001, ZQ3)
-- for rounding off after the first hop
type ZQ5 = (Zq 3144961, ZQ4)
type ZQ6 = (Zq 7338241, ZQ5)


type Zq (q :: k) = ZqBasic q Z
type Z = Int64

type ZP2 = Zq PP2
--type ZP3 = Zq PP3
type ZP4 = Zq PP4
type ZP8 = Zq PP8
{-
-- odd primes < 20
primes = [3,5,7,11,13,17,19]


-- generates a sequence of len hops (len+1 rings)
--genGoodSeqs :: Int -> Int -> [[Int]]
genGoodSeqs r len = do
  -- a sequence of ss with an implicit "1" at the start
  let ss = map (scanl1 (*)) $ sequence $ replicate len primes
  let allSeqs = map (genHopRngs r 1) ss
  let validSeqs = catMaybes allSeqs
      goodSeqs = (map (r :) $ filter ((<= 12) . maxOrd2) validSeqs)
      getEs xs = map (uncurry gcd) $ zip xs (tail xs)
      goodEs = map getEs goodSeqs
      crtSizes = zipWith (\es hs -> zipWith crtSetSize es hs) goodEs (map tail goodSeqs) :: [[Int]]

  mapM_ printFactors goodSeqs
  mapM_ printList crtSizes
  mapM_ printList $ map ((map (order2 . twoFree)) . tail) goodSeqs
  putStrLn "done"


-- takes R and Ss and outputs Hs
genHopRngs :: Int -> Int -> [Int] -> Maybe [Int]
genHopRngs 1 lasts _ = Just [] -- we dont' need any more slots
genHopRngs _ _ [] = Nothing  -- we need more slots but don't have more primes
genHopRngs r lasts (s:ss) = do
  let usableSlots = 2^(floor ((log $ fromIntegral $ crtSetSize lasts s)/(log 2)::Double))
  if usableSlots > 1
  then let r' = if r == 4
                then 1
                else r `div` usableSlots
           h = r' * s
       in fmap (h :) $ genHopRngs r' s ss
  else Nothing

test = genRingHopInfo 5 (2^31)
  [128,64,32,8,4,1]
  [1,7,7*13,5*7*13,3*5*7*13,9*5*7*13]
  [7*13,1, 1,1,1,1]

genRingHopInfo :: Int -> Int -> [Int] -> [Int] -> [Int] -> IO ()
genRingHopInfo numMods lbd rs ss pads
  | length rs == length ss && length ss == length pads = do
    let hs = zipWith (*) rs ss
        listToPairs [x] = []
        listToPairs (x:y:zs) = (x,y) : (listToPairs (y:zs))
        pgcd (a,b) = gcd a b
        es = map pgcd $ listToPairs hs
        hs' = ((head pads) * (head hs)) : [(pads !! i)*(lcm (es' !! (i-1)) (hs !! i)) | i <- [1..((length rs)-1)]]
        es' = [(es !! i) * (hs' !! i) `div` (hs !! i) | i <- [0..((length rs)-2)]]
        sanityCheck = and $ [gcd ((hs' !! i) `div` (hs !! i)) ((hs !! i) `div` (es !! i)) == 1 | i <- [0..((length rs)-1)]]
        hname i = "H" ++ (show i)
        h'name i = "H" ++ (show i) ++ "'"
        compositumDim = foldl1' lcm hs'
        totm = totientInt (head hs)
        -- check that this ring switch schedule results in enough CRT slots for rounding
        crtSlots = zipWith crtSetSize es hs
    when (totm > product crtSlots) $ error $ "not enough slots: need " ++ (show totm) ++ ", have " ++ (show crtSlots)
    --mapM (putStrLn . (printType hname)) $ zip [0..] (map (factorise . fromIntegral) hs)
    --mapM (putStrLn . (printType h'name)) $ zip [0..] (map (factorise . fromIntegral) hs')
    --mapM (putStrLn . printMod) $ zip [1..] $ take numMods $ goodQs compositumDim lbd
    --print $ "-- " ++ (show hs')
    print $ "Compositum index " ++ (show compositumDim)
    putStrLn $ (show crtSlots) ++ "\t=" ++ (show $ product crtSlots) ++ ", need " ++ (show $ totientInt (head hs))
    putStrLn $ "Ord2 Ss: " ++ (show $ map (order2 . twoFree) $ tail hs)
    putStrLn $ "ct dims: " ++ (show $ map totientInt hs')
    putStrLn $ show hs'

    let mods = (take 3 $ goodQs compositumDim (2^24)) ++
               [head $ goodQs compositumDim (2^21),
                head $ goodQs compositumDim (2^15)]
    --mapM (putStrLn . printMod) $ zip [1..] mods

    return ()

printFactors :: [Int] -> IO ()
printFactors xs =
  printList $
    map ((map (\(p,e) -> fromInteger (p^(fromIntegral e)) :: Int)) . factorise . fromIntegral) xs

printList :: (Show a) => [a] -> IO ()
printList xs = do
  putStrLn $ show xs
  return ()


maxOrd2 :: [Int] -> Int
maxOrd2 = maximum . (map (order2 . twoFree))

sumOrd2 :: [Int] -> Int
sumOrd2 = sum . (map (order2 . twoFree))

-- good Q is one where Q \equiv 1 `mod` m
isGoodQ m q = q == (head $ goodQs m (q-1))

-- an infinite list of primes greater than the input and congruent to
-- 1 mod m
goodQs :: (ToInteger i) => i -> i -> [i]
goodQs m lower = checkVal (lower + ((m-lower) `mod` m) + 1)
  where checkVal v = if (isPrime (fromIntegral v :: Integer))
                     then v : checkVal (v+m)
                     else checkVal (v+m)

totientInt :: Int -> Int
totientInt =
  let totpp (p'',e) = let p' = fromInteger p'' in (p'-1)*p'^(fromIntegral $ e-1) :: Int
  in product . map totpp . factorise . fromIntegral

printNat :: Int -> String
printNat i = "N" ++ (show i)

printPP :: (Integer, Int) -> String
printPP (p,e) = "'PP '(" ++ (printNat $ fromInteger p) ++ ", " ++ (printNat e) ++ ")"

printF :: [(Integer, Int)] -> String
printF pps = "'F '[ " ++ (printpps pps) ++ " ]"
  where printpps [pp] = printPP pp
        printpps (pp:pps) = (printPP pp) ++ ", " ++ (printpps pps)

printType :: (Int -> String) -> (Int, [(Integer,Int)]) -> String
printType pre (i,factors) = "type " ++ (pre i) ++ " = " ++ (printF factors)

printMod :: (Int, Int) -> String
printMod (i,q) =
  "data Q" ++ (show q) ++
  " deriving (Typeable)\ninstance (ToInteger i) => Reflects Q" ++
  (show q) ++ " i where value = return " ++ (show q) ++ "\n" ++ (printZq i q)

printZq :: Int -> Int -> String
printZq 1 q = "type ZQ1 = Zq Q" ++ (show q) ++ "\n"
printZq i q = "type ZQ" ++ (show i) ++ " = (Zq Q" ++ (show q) ++ ", ZQ" ++ (show (i-1)) ++ ")\n"

-- order of 2 in Z_q^*
order2 :: Int -> Int
order2 q = length $ 1 : (takeWhile (> 1) $ iterate (\x -> (x*2) `mod` q) (2 `mod` q))

-- removes all factors of 2 from k
twoFree :: Int -> Int
twoFree k = head $ dropWhile (\x -> (x `mod` 2) == 0) $ iterate (\x -> x `div` 2) k

crtSetSize :: Int -> Int -> Int
crtSetSize m m' =
  let p = 2 :: Int
      mbar = twoFree m
      m'bar = twoFree m'
      totm = totientInt m
      totm' = totientInt m'
      -- order of 2 in Zq*
      ord2m = order2 mbar
      ord2m' = order2 m'bar
  in (totm' * ord2m) `div` (totm * ord2m')
-}




-- timing functionality
time :: (NFData a) => String -> a -> IO a
time s m = do
  putStr' s
  wallStart <- getCurrentTime
  m `deepseq` printTimes Nothing wallStart 1
  return m

-- flushes the print buffer
putStr' :: String -> IO ()
putStr' str = do
  putStr str
  hFlush stdout

--rounds to precision, for pretty printing
toPrecision :: (Field a, RealRing a) => Integer -> a -> a
toPrecision n x = (fromInteger $ round $ x * (10^n)) / (10.0^n)

printTimes :: Maybe Bool -> UTCTime -> Int -> IO ()
printTimes _ wallStart iters = do
    wallEnd <- getCurrentTime
    let wallTime = realToFrac $ diffUTCTime wallEnd wallStart :: Double
    putStr $ "Wall time: " ++ (show $ toPrecision 2 $ wallTime) ++ "s"
    if iters == 1
    then putStr "\n\n"
    else putStr $ "\tAverage wall time: " ++ (show $ toPrecision 2 $ wallTime / (fromIntegral iters)) ++ "s\n\n"


instance NFData Dynamic where
  rnf _ = ()
