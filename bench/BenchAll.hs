{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Criterion.Main
import Criterion.Types (Config(..))

import Control.DeepSeq
import Prelude hiding (lookup, max, foldr)
import System.Random
import Data.Foldable (foldr)
import Data.List (sort)

import Data.IntervalMap.Interval
import qualified Data.Map as D
import qualified Data.IntervalMap as M
import qualified Data.IntervalMap.Generic.Lazy as G


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
benchConfig =  defaultConfig { reportFile = Just "bench-all.html" }

cDATA_SIZE :: Int
cDATA_SIZE =  10000

instance Ord a => G.Interval (a,a) a where
  lowerBound (a,_) = a
  upperBound (_,a) = a


main :: IO ()
main =
  do
      let ivs  = genRandomIntervals cDATA_SIZE 20 cDATA_SIZE
      let ivs2 = genRandomIntervals cDATA_SIZE 20 (cDATA_SIZE `quot` 2)
      ivsP   <- ensure $ [(ClosedInterval lo hi, lo) | (lo,hi) <- ivs]
      ivsG   <- ensure $ [(iv,lo) | iv@(lo,_) <- ivs]
      oIvsP  <- ensure $ sort ivsP
      oIvsG  <- ensure $ sort ivsG
      lookupKeys <- ensure $ [i | (i,_) <- ivsP]
      lookupKeysG <- ensure $ [i | (i,_) <- ivsG]
      dMap   <- ensure $ D.fromList ivsP
      dIvMap <- ensure $ M.fromList ivsP
      gIvMap <- ensure $ G.fromList ivsG
      dMapSmall <- ensure $ D.fromList [(ClosedInterval lo hi, lo) | (lo,hi) <- ivs2]
      dIvMapSmall <- ensure $ M.fromList [(ClosedInterval lo hi, lo) | (lo,hi) <- ivs2]
      rndInts <- ensure (genRandomInts 1 cDATA_SIZE cDATA_SIZE)
      defaultMainWith benchConfig [
         bgroup "fromList (insert)" [
           bench "Data.Map"        $ nf D.fromList ivsP,
           bench "IntervalMap"     $ nf M.fromList ivsP,
           bench "IMGeneric"       $ nf G.fromList ivsG
         ],
         bgroup "fromAscList" [
           bench "Data.Map"        $ nf D.fromAscList oIvsP,
           bench "IntervalMap"     $ nf M.fromAscList oIvsP,
           bench "IMGeneric"       $ nf G.fromAscList oIvsG
         ],
         bgroup "search" [
           bench "lookup Data.Map" $ nf (\m -> [D.lookup i m | i <- lookupKeys]) dMap,
           bench "lookup"          $ nf (\m -> [M.lookup i m | i <- lookupKeys]) dIvMap,
           bench "containing"      $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `M.containing` p]) dIvMap,
           bench "intersecting"    $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `M.intersecting` (ClosedInterval p (p+15))]) dIvMap,
           bench "within" $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `M.within` (ClosedInterval p (p+15))]) dIvMap,
           bench "G lookup"          $ nf (\m -> [G.lookup i m | i <- lookupKeysG]) gIvMap,
           bench "G containing"      $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `G.containing` p]) gIvMap,
           bench "G intersecting"    $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `G.intersecting` (p, p+15)]) gIvMap,
           bench "G within" $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `G.within` (p, p+15)]) gIvMap
         ],
         bgroup "mapKeys" [
           bench "Data.Map"              $ nf (D.mapKeys (move 1)) dMap,
           bench "IntervalMap"           $ nf (M.mapKeys (move 1)) dIvMap,
           bench "Data.Map monotonic"    $ nf (D.mapKeysMonotonic (move 1)) dMap,
           bench "IntervalMap monotonic" $ nf (M.mapKeysMonotonic (move 1)) dIvMap
         ],
         bgroup "map" [
           bench "Data.Map"    $ nf (D.map (1+)) dMap,
           bench "IntervalMap" $ nf (M.map (1+)) dIvMap
         ],
         bgroup "union" [
           bench "Data.Map Large/Small"    $ nf (\m -> D.union m dMapSmall) dMap,
           bench "Data.Map Small/Large"    $ nf (\m -> D.union dMapSmall m) dMap,
           bench "IntervalMap Large/Small" $ nf (\m -> M.union m dIvMapSmall) dIvMap,
           bench "IntervalMap Small/Large" $ nf (\m -> M.union dIvMapSmall m) dIvMap,
           bench "Data.Map Large/Empty"    $ nf (\m -> D.union m D.empty) dMap,
           bench "Data.Map Empty/Large"    $ nf (\m -> D.union D.empty m) dMap,
           bench "IntervalMap Large/Empty" $ nf (\m -> M.union m M.empty) dIvMap,
           bench "IntervalMap Empty/Large" $ nf (\m -> M.union M.empty m) dIvMap,
           bench "IMGeneric Large/Empty" $ nf (\m -> G.union m G.empty) gIvMap,
           bench "IMGeneric Empty/Large" $ nf (\m -> G.union G.empty m) gIvMap,
           bench "Data.Map self"    $ nf (\m -> D.union m m) dMap,
           bench "IntervalMap self" $ nf (\m -> M.union m m) dIvMap
         ],
         bgroup "intersection" [
           bench "Data.Map Large/Small"    $ nf (\m -> D.intersection m dMapSmall) dMap,
           bench "Data.Map Small/Large"    $ nf (\m -> D.intersection dMapSmall m) dMap,
           bench "IntervalMap Large/Small" $ nf (\m -> M.intersection m dIvMapSmall) dIvMap,
           bench "IntervalMap Small/Large" $ nf (\m -> M.intersection dIvMapSmall m) dIvMap
         ],
         bgroup "difference" [
           bench "Data.Map"    $ nf (\m -> D.difference m dMapSmall) dMap,
           bench "IntervalMap" $ nf (\m -> M.difference m dIvMapSmall) dIvMap
         ],
         bgroup "delete" [
           bench "deleteMin Data.Map"    $ nf (bogoSize D.null D.deleteMin) dMap,
           bench "deleteMax Data.Map"    $ nf (bogoSize D.null D.deleteMax) dMap,
           bench "deleteMin IntervalMap" $ nf (bogoSize M.null M.deleteMin) dIvMap,
           bench "deleteMax IntervalMap" $ nf (bogoSize M.null M.deleteMax) dIvMap,
           bench "minView Data.Map"      $ nf (unfold D.minView) dMap,
           bench "maxView Data.Map"      $ nf (unfold D.maxView) dMap,
           bench "minView IntervalMap"   $ nf (unfold M.minView) dIvMap,
           bench "maxView IntervalMap"   $ nf (unfold M.maxView) dIvMap,
           bench "delete Data.Map"       $ nf (\m -> foldr (\k mp -> D.delete k mp) m lookupKeys) dMap,
           bench "delete IntervalMap"    $ nf (\m -> foldr (\k mp -> M.delete k mp) m lookupKeys) dIvMap
         ]
       ]

bogoSize :: (m -> Bool) -> (m -> m) -> m -> Int
bogoSize isEmpty shrink d = go 0 d
  where go r m | isEmpty m    = r
               | otherwise = go (r + 1) (shrink m)

unfold :: (m -> Maybe (x,m)) -> m -> [x]
unfold view m = case view m of
                  Nothing     -> []
                  Just (x,m') -> x : unfold view m'
