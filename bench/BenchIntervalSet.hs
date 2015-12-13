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
import qualified Data.IntervalSet as S


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
benchConfig =  defaultConfig { reportFile = Just "bench-set.html" }

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
      ivsP   <- ensure $ [IV lo hi | (lo,hi) <- ivs]
      oIvsP  <- ensure $ sort ivsP
      let lookupKeys = ivsP
      set <- ensure $ S.fromList ivsP
      rndInts <- ensure (genRandomInts 1 cDATA_SIZE cDATA_SIZE)
      defaultMainWith benchConfig [
         bgroup "fromList" [
           bench "fromList"    $ nf S.fromList ivsP,
           bench "fromAscList" $ nf S.fromAscList oIvsP
         ],
         bgroup "search" [
           bench "size"         $ nf S.size set,
           bench "member"       $ nf (\m -> [S.member i m | i <- lookupKeys]) set,
           bench "containing"   $ nf (\m -> sum [v | p <- rndInts, IV v _ <- S.toList (m `S.containing` p)]) set,
           bench "intersecting" $ nf (\m -> sum [v | p <- rndInts, IV v _ <- S.toList (m `S.intersecting` (IV p (p+15)))]) set,
           bench "within"       $ nf (\m -> sum [v | p <- rndInts, IV v _ <- S.toList (m `S.within` (IV p (p+15)))]) set
         ],
         bgroup "mapKeys" [
           bench "mapKeys"           $ nf (S.map (move 1)) set,
           bench "monotonic" $ nf (S.mapMonotonic (move 1)) set
         ]
       ]
