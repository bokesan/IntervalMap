{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Criterion.Main
import Criterion.Types (Config(..))

import Control.DeepSeq
import Prelude hiding (lookup, max, foldr)
import System.Random
import Data.Foldable (foldr)
import Data.List (sort)

import Data.IntervalMap.Generic.Interval
import qualified Data.IntervalMap.Generic.Lazy as L
import qualified Data.IntervalMap.Generic.Strict as S


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


benchConfig :: Config
benchConfig =  defaultConfig { reportFile = Just "bench-lazy-strict.html" }

cDATA_SIZE :: Int
cDATA_SIZE =  10000

data IV = IV {-# UNPACK #-} !Int {-# UNPACK #-} !Int
          deriving (Eq, Ord)

instance NFData IV where
  rnf a = a `seq` ()

instance Interval IV Int where
  lowerBound (IV l _) = l
  upperBound (IV _ u) = u

move :: Int -> IV -> IV
move n (IV a b) = IV (a+n) (b+n)


main :: IO ()
main =
  do
      let ivs  = genRandomIntervals cDATA_SIZE 20 cDATA_SIZE
      ivsP   <- ensure $ [(IV lo hi, lo) | (lo,hi) <- ivs]
      oIvsP  <- ensure $ sort ivsP
      lookupKeys <- ensure $ [i | (i,_) <- ivsP]
      sMap <- ensure $ S.fromList ivsP
      lMap <- ensure $ L.fromList ivsP
      rndInts <- ensure (genRandomInts 1 cDATA_SIZE cDATA_SIZE)
      defaultMainWith benchConfig [
         bgroup "fromList (insert)" [
           bench "Lazy"     $ nf L.fromList ivsP,
           bench "Strict"   $ nf S.fromList ivsP
         ],
         bgroup "fromAscList" [
           bench "Lazy"     $ nf L.fromAscList oIvsP,
           bench "Strict"   $ nf S.fromAscList oIvsP
         ],
         bgroup "search" [
           bench "lookup lazy"          $ nf (\m -> [L.lookup i m | i <- lookupKeys]) lMap,
           bench "lookup strict"        $ nf (\m -> [S.lookup i m | i <- lookupKeys]) sMap,
           bench "containing lazy"      $ nf (\m -> sum [v | p <- rndInts, v <- L.elems (m `L.containing` p)]) lMap,
           bench "containing strict"      $ nf (\m -> sum [v | p <- rndInts, v <- S.elems (m `S.containing` p)]) sMap,
           bench "intersecting lazy"    $ nf (\m -> sum [v | p <- rndInts, v <- L.elems (m `L.intersecting` (IV p (p+15)))]) lMap,
           bench "intersecting strict"    $ nf (\m -> sum [v | p <- rndInts, v <- S.elems (m `S.intersecting` (IV p (p+15)))]) sMap,
           bench "within lazy" $ nf (\m -> sum [v | p <- rndInts, v <- L.elems (m `L.within` (IV p (p+15)))]) lMap,
           bench "within strict" $ nf (\m -> sum [v | p <- rndInts, v <- S.elems (m `S.within` (IV p (p+15)))]) sMap
         ],
         bgroup "mapKeys" [
           bench "lazy"             $ nf (L.mapKeys (move 1)) lMap,
           bench "strict"           $ nf (S.mapKeys (move 1)) sMap,
           bench "monotonic lazy"   $ nf (L.mapKeysMonotonic (move 1)) lMap,
           bench "monotonic strict" $ nf (S.mapKeysMonotonic (move 1)) sMap
         ],
         bgroup "map" [
           bench "lazy"    $ nf (L.map (1+)) lMap,
           bench "strict"  $ nf (S.map (1+)) sMap
         ],
         bgroup "union" [
           bench "Large/Empty lazy"   $ nf (\m -> L.union m L.empty) lMap,
           bench "Large/Empty strict" $ nf (\m -> S.union m S.empty) sMap,
           bench "Empty/Large lazy"   $ nf (\m -> L.union L.empty m) lMap,
           bench "Empty/Large strict" $ nf (\m -> S.union S.empty m) sMap,
           bench "self lazy"          $ nf (\m -> L.union m m) lMap,
           bench "self strict"        $ nf (\m -> S.union m m) sMap
         ]
       ]
