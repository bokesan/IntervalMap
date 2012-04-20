import Criterion.Main
import Criterion.Config

import Control.DeepSeq
import Prelude hiding (lookup, max, foldr)
import System.Random
import Data.Foldable (foldr)

import Data.Map as D
import Data.IntervalMap as M hiding (foldr)


seed :: Int
seed = 54321

ensure :: NFData a => a -> IO a
ensure xs = xs `deepseq` return xs

forceRange :: Int -> Int -> Int -> Int
forceRange lo hi n | n >= lo && n <= hi = n
                   | n < 0              = forceRange lo hi (0 - n)
                   | otherwise          = lo + (n `rem` (1 + hi - lo))

genRandomInts :: Int -> Int -> Int -> [Int]
genRandomInts lo hi n = Prelude.map (forceRange lo hi) . take n . randoms . mkStdGen $ seed

genRandomIntervals :: Int -> Int -> Int -> [(Int,Int)]
genRandomIntervals max lap n = genIvs . take (2*n) . randoms . mkStdGen $ seed
  where
    genIvs [] = []
    genIvs [_] = []
    genIvs (x:y:xs) = let lo = forceRange 1 max x
                          sz = forceRange 0 lap y
                      in (lo, lo + sz) : genIvs xs

move :: Int -> M.Interval Int -> M.Interval Int
move n = fmap (n+) 

-- always write a report to bench-all.html.
benchConfig :: Config
benchConfig =  defaultConfig { cfgReport = ljust "bench-all.html" }


main :: IO ()
main =
  do
      let ivs  = genRandomIntervals 100000 20 100000
      let ivs2 = genRandomIntervals 100000 20  20000
      lookupKeys <- ensure $ [ClosedInterval lo hi | (lo,hi) <- take 10000  ivs]
      m1e5 <- ensure $ D.fromList [(ClosedInterval lo hi, lo) | (lo,hi) <- ivs]
      i1e5 <- ensure $ M.fromList [(ClosedInterval lo hi, lo) | (lo,hi) <- ivs]
      m2e4 <- ensure $ D.fromList [(ClosedInterval lo hi, lo) | (lo,hi) <- ivs2]
      i2e4 <- ensure $ M.fromList [(ClosedInterval lo hi, lo) | (lo,hi) <- ivs2]
      r1e4 <- ensure (genRandomInts 1 100000 10000)
      defaultMainWith benchConfig (return ()) [
         bgroup "search" [
           bench "lookup Data.Map" $ nf (\m -> [D.lookup i m | i <- lookupKeys]) m1e5,
           bench "lookup"          $ nf (\m -> [M.lookup i m | i <- lookupKeys]) i1e5,
           bench "containing"      $ nf (\m -> sum [v | p <- r1e4, (_,v) <- m `M.containing` p]) i1e5,
           bench "intersecting"    $ nf (\m -> sum [v | p <- r1e4, (_,v) <- m `M.intersecting` (ClosedInterval p (p+15))]) i1e5,
           bench "within" $ nf (\m -> sum [v | p <- r1e4, (_,v) <- m `M.within` (ClosedInterval p (p+15))]) i1e5
         ],
         bgroup "mapKeys" [
           bench "2e4 Data.Map"              $ nf (D.mapKeys (move 1)) m2e4,
           bench "2e4 IntervalMap"           $ nf (M.mapKeys (move 1)) i2e4,
           bench "1e5 Data.Map monotonic"    $ nf (D.mapKeysMonotonic (move 1)) m1e5,
           bench "1e5 IntervalMap monotonic" $ nf (M.mapKeysMonotonic (move 1)) i1e5
         ],
         bgroup "map" [
           bench "Data.Map"    $ nf (D.map (1+)) m1e5,
           bench "IntervalMap" $ nf (M.map (1+)) i1e5
         ],
         bgroup "union" [
           bench "Data.Map"         $ nf (\m -> D.union m m2e4) m1e5,
           bench "Data.Map flip"    $ nf (\m -> D.union m2e4 m) m1e5,
           bench "IntervalMap"      $ nf (\m -> M.union m i2e4) i1e5,
           bench "IntervalMap flip" $ nf (\m -> M.union i2e4 m) i1e5
         ],
         bgroup "difference" [
           bench "Data.Map"    $ nf (\m -> D.difference m m2e4) m1e5,
           bench "IntervalMap" $ nf (\m -> M.difference m i2e4) i1e5
         ],
         bgroup "intersection" [
           bench "Data.Map"    $ nf (\m -> D.intersection m m2e4) m1e5,
           bench "IntervalMap" $ nf (\m -> M.intersection m i2e4) i1e5
         ],
         bgroup "delete" [
           bench "Data.Map"    $ nf (\m -> foldr (\k mp -> D.delete k mp) m lookupKeys) m1e5,
           bench "IntervalMap" $ nf (\m -> foldr (\k mp -> M.delete k mp) m lookupKeys) i1e5
         ]
       ]
