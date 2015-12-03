{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Criterion.Main
import Criterion.Types (Config(..))

import Control.DeepSeq
import Prelude hiding (lookup, max, foldr)
import System.Random
import Data.List (sort)

import Data.IntervalMap.Generic.Interval
import qualified Data.IntervalMap.Generic.Strict as S
import qualified RBColorInt as L
import qualified RBColorNode as N

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
benchConfig =  defaultConfig { reportFile = Just "bench-rb-impl.html" }

cDATA_SIZE :: Int
cDATA_SIZE =  500000

cTEST_SIZE :: Int
cTEST_SIZE =  25000

data IV = IV {-# UNPACK #-} !Int {-# UNPACK #-} !Int
          deriving (Eq, Ord)

instance NFData IV where
  rnf a = a `seq` ()

instance Interval IV Int where
  lowerBound (IV l _) = l
  upperBound (IV _ u) = u


main :: IO ()
main =
  do
      let ivs  = genRandomIntervals cDATA_SIZE 20 cDATA_SIZE
      ivsP   <- ensure $ [(IV lo hi, lo) | (lo,hi) <- ivs]
      oIvsP  <- ensure $ sort ivsP
      lookupKeys <- ensure $ take cTEST_SIZE [i | (i,_) <- ivsP]
      sMap <- ensure $ S.fromAscList oIvsP
      lMap <- ensure $ L.fromAscList oIvsP
      nMap <- ensure $ N.fromAscList oIvsP
      rndInts <- ensure (genRandomInts 1 cDATA_SIZE cTEST_SIZE)
      defaultMainWith benchConfig [
         bgroup "fromAscList" [
           bench "regular" $ nf S.fromAscList oIvsP,
           bench "int"     $ nf L.fromAscList oIvsP,
           bench "node"    $ nf N.fromAscList oIvsP
         ],
         bgroup "lookup" [
           bench "reg"    $ nf (\m -> [S.lookup i m | i <- lookupKeys]) sMap,
           bench "int"    $ nf (\m -> [L.lookup i m | i <- lookupKeys]) lMap,
           bench "node"   $ nf (\m -> [N.lookup i m | i <- lookupKeys]) nMap
         ],
         bgroup "containing" [
           bench "reg"    $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `S.containing` p]) sMap,
           bench "int"    $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `L.containing` p]) lMap,
           bench "node"   $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `N.containing` p]) nMap
         ],
         bgroup "intersecting" [
           bench "reg"    $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `S.intersecting` (IV p (p+15))]) sMap,
           bench "int"    $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `L.intersecting` (IV p (p+15))]) lMap,
           bench "node"   $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `N.intersecting` (IV p (p+15))]) nMap
         ],
         bgroup "within" [
           bench "reg"    $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `S.within` (IV p (p+15))]) sMap,
           bench "int"    $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `L.within` (IV p (p+15))]) lMap,
           bench "node"   $ nf (\m -> sum [v | p <- rndInts, (_,v) <- m `N.within` (IV p (p+15))]) nMap
         ]
       ]
