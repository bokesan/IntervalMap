{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Criterion.Main
import Criterion.Types (Config(..), Verbosity(..))

import Control.DeepSeq
import Prelude hiding (lookup, max, foldr)
import System.Random
import Data.List (nub)
import Data.Foldable (foldr)
import Data.Maybe

import IntRange
import qualified Data.IntervalMap.Generic.Strict as RB
import qualified IvMapSortedList as SL
import qualified Data.IntervalMap.FingerTree as FT


instance Ord a => Interval (FT.Interval a) a where
  lowerBound = FT.low
  upperBound = FT.high

instance NFData a => NFData (FT.Interval a) where
  rnf (FT.Interval a b) = a `deepseq` b `deepseq` ()

instance (NFData k, NFData v) => NFData (FT.IntervalMap k v) where
  -- FIXME
  rnf a = a `seq` ()

ftFromList :: Ord k => [(FT.Interval k, v)] -> FT.IntervalMap k v
ftFromList =  foldr (\(k,v) m -> FT.insert k v m) FT.empty

cSEED, cSEED2 :: Int
cSEED  = 54321
cSEED2 = 12345

forceRange :: Int -> Int -> Int -> Int
forceRange lo hi n | n >= lo && n <= hi = n
                   | n < 0              = forceRange lo hi (0 - n)
                   | otherwise          = lo + (n `rem` (1 + hi - lo))

genRandomIntervals :: Int -> Int -> Int -> [IntRange]
genRandomIntervals = genRandomIntervalsWithSeed cSEED

genRandomIntervalsWithSeed :: Int -> Int -> Int -> Int -> [IntRange]
genRandomIntervalsWithSeed seed max lap n = genIvs . take (2*n) . randoms . mkStdGen $ seed
  where
    genIvs [] = []
    genIvs [_] = []
    genIvs (x:y:xs) = let lo = forceRange 1 max x
                          sz = forceRange 0 lap y
                      in IntRange lo (lo + sz) : genIvs xs


benchConfig :: Config
benchConfig =  defaultConfig { reportFile = Just "bench-compare-types.html", verbosity = Verbose }

cNUM_KEYS :: Int
cNUM_KEYS =  10

mkKeys :: Int -> [IntRange]
mkKeys n =  genRandomIntervalsWithSeed cSEED2 n 18 cNUM_KEYS ++ [IntRange 0 1, IntRange (n+1) (n+2)]

slEnv :: Int -> IO ([IntRange], SL.IVS IntRange Int)
slEnv n = do
   let ivs = genRandomIntervals n 20 n
   return (mkKeys n, SL.fromList [(iv, lowerBound iv) | iv <- ivs])

rbEnv :: Int -> IO ([IntRange], RB.IntervalMap IntRange Int)
rbEnv n = do
   let ivs = genRandomIntervals n 20 n
   return (mkKeys n, RB.fromList [(iv, lowerBound iv) | iv <- ivs])

ftEnv :: Int -> IO ([FT.Interval Int], FT.IntervalMap Int Int)
ftEnv n = do
   let ivs  = genRandomIntervals n 20 n
   let ivsFt = [(FT.Interval lo hi, lo) | (IntRange lo hi) <- nub ivs]
   return (map toIv (mkKeys n), ftFromList ivsFt)
  where
   toIv (IntRange a b) = FT.Interval a b


runInsert :: Interval k a => (k -> a -> m -> m) -> m -> [k] -> m
runInsert ins m keys = foldr (\k mp -> ins k (lowerBound k) mp) m keys

runLookup :: (k -> m -> Maybe Int) -> m -> [k] -> Int
runLookup f m keys = sum $ catMaybes $ map (\k -> f k m) keys

runContains :: (Interval i e) => (t -> e -> [(t1, Int)]) -> t -> [i] -> Int
runContains f m keys = sum [v | k <- keys, (_,v) <- f m (lowerBound k)]

benchIV name ins look cont ~(keys,m) = bgroup name [
    bench "insert"      $ whnf (runInsert ins m) keys,
    bench "lookup"      $ nf (runLookup look m) keys,
    bench "containing"  $ nf (runContains cont m) keys
  ]

benchSL :: ([IntRange], SL.IVS IntRange Int) -> Benchmark
benchSL = benchIV "SortedList" SL.insert SL.lookup SL.containing

benchRB :: ([IntRange], RB.IntervalMap IntRange Int) -> Benchmark
benchRB = benchIV "RedBlackTree" RB.insert RB.lookup (\m p -> RB.toAscList (RB.containing m p))

benchFT :: ([FT.Interval Int], FT.IntervalMap Int Int) -> Benchmark
benchFT ~(keys,m) = bgroup "FingerTree" [
    bench "insert"      $ whnf (runInsert FT.insert m) keys,
    bench "containing"  $ nf (\ks -> sum [v | k <- ks, (_,v) <- FT.search (FT.low k) m]) keys
  ]


main :: IO ()
main = defaultMainWith benchConfig [
        bgroup "10" [
           env (slEnv 10) benchSL,
           env (rbEnv 10) benchRB,
           env (ftEnv 10) benchFT
         ],
         bgroup "100" [
           env (slEnv 100) benchSL,
           env (rbEnv 100) benchRB,
           env (ftEnv 100) benchFT
         ],
         bgroup "1000" [
           env (slEnv 1000) benchSL,
           env (rbEnv 1000) benchRB,
           env (ftEnv 1000) benchFT
         ],
         bgroup "2500" [
           env (slEnv 2500) benchSL,
           env (rbEnv 2500) benchRB,
           env (ftEnv 2500) benchFT
         ],
         bgroup "10000" [
           env (rbEnv 10000) benchRB,
           env (ftEnv 10000) benchFT
         ],
         bgroup "20000" [
           env (rbEnv 20000) benchRB,
           env (ftEnv 20000) benchFT
         ],
         bgroup "50000" [
           env (rbEnv 50000) benchRB,
           env (ftEnv 50000) benchFT
         ],
         bgroup "100000" [
           env (rbEnv 100000) benchRB,
           env (ftEnv 100000) benchFT
         ]
       ]
