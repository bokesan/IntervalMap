{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Criterion.Main
import Criterion.Types (Config(..))

import Control.DeepSeq
import Prelude hiding (lookup, max, foldr)
import System.Random
import Data.Foldable (foldr)
import Data.List (sort, foldl')

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
      let ivs  = genRandomIntervals cDATA_SIZE 50 cDATA_SIZE
      ivsP   <- ensure $ [IV lo hi | (lo,hi) <- ivs]
      oIvsP  <- ensure $ sort ivsP
      let lookupKeys = ivsP
      set <- ensure $ S.fromList ivsP
      rndInts <- ensure (genRandomInts 1 cDATA_SIZE cDATA_SIZE)
      rndInts2 <- ensure (take (2 * log2 cDATA_SIZE) rndInts)
      rndIvs <- ensure [IV (p - 500) (p + 500) | p <- rndInts2]
      defaultMainWith benchConfig [
         bgroup "fromList" [
           bench "fromList"     $ nf S.fromList ivsP,
           bench "fromAscList"  $ nf S.fromAscList oIvsP
         ],
         bench "size"           $ whnf S.size set,
         bench "member"         $ whnf (\s -> sum [bval (S.member i s) | i <- lookupKeys]) set,
         bgroup "points" [
           bench "containing"   $ whnf (\s -> sum [maxValue (S.containing s p) | p <- rndInts]) set,
           bench "splitAt lo"   $ whnf (\s -> sum [maxValue (splitAt1 s p) | p <- rndInts2]) set,
           bench "splitAt c"    $ whnf (\s -> sum [maxValue (splitAt2 s p) | p <- rndInts]) set,
           bench "splitAt hi"   $ whnf (\s -> sum [minValue (splitAt3 s p) | p <- rndInts2]) set
         ],
         bgroup "intervals" [
           bench "intersecting" $ whnf (\s -> sum [maxValue (s `S.intersecting` i) | i <- rndIvs]) set,
           bench "within"       $ whnf (\s -> sum [maxValue (s `S.within`       i) | i <- rndIvs]) set,
           bench "spi1"         $ whnf (\s -> sum [maxValue (spi1 s i) | i <- rndIvs]) set,
           bench "spi2"         $ whnf (\s -> sum [maxValue (spi2 s i) | i <- rndIvs]) set,
           bench "spi3"         $ whnf (\s -> sum [maxValue (spi3 s i) | i <- rndIvs]) set
         ],
         bgroup "map" [
           bench "map"      $ nf (S.map (move 1)) set,
           bench "monotonic" $ nf (S.mapMonotonic id) set
         ]
       ]


log2 :: Int -> Int
log2 m = h (-1) m
  where
    h r n | r `seq` n <= 0 = r
          | otherwise      = h (r + 1) (n `quot` 2)

bval :: Bool -> Int
bval False = 0
bval True  = 1

minValue :: S.IntervalSet IV -> Int
minValue s = case S.findMin s of
               Nothing -> 0
               Just (IV lo _) -> lo

maxValue :: S.IntervalSet IV -> Int
maxValue s = case S.findMax s of
               Nothing -> 0
               Just (IV lo _) -> lo
             
splitAt1, splitAt2, splitAt3 :: (Interval i e, Ord i) => S.IntervalSet i -> e -> S.IntervalSet i
splitAt1 s p = case S.splitAt s p of (lo,_,_) -> lo
splitAt2 s p = case S.splitAt s p of (_,c,_) -> c
splitAt3 s p = case S.splitAt s p of (_,_,hi) -> hi

spi1, spi2, spi3 :: (Interval i e, Ord i) => S.IntervalSet i -> i -> S.IntervalSet i
spi1 s i = case S.splitIntersecting s i of (x,_,_) -> x
spi2 s i = case S.splitIntersecting s i of (_,x,_) -> x
spi3 s i = case S.splitIntersecting s i of (_,_,x) -> x
