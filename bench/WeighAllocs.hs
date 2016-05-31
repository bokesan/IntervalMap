{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Weigh

import Control.DeepSeq
import Prelude hiding (lookup, max, foldr)
import System.Random
import Data.Foldable (foldr)

import Data.IntervalMap.Generic.Interval
import qualified Data.IntervalSet as S
import qualified Data.Set as C


seed :: Int
seed = 54321

ensure :: NFData a => a -> IO a
ensure xs = xs `deepseq` return xs

forceRange :: Int -> Int -> Int -> Int
forceRange lo hi n | n >= lo && n <= hi = n
                   | n < 0              = forceRange lo hi (0 - n)
                   | otherwise          = lo + (n `rem` (1 + hi - lo))

genRandomIntervals :: Int -> Int -> Int -> [(Int,Int)]
genRandomIntervals max lap n = genIvs . take (2*n) . randoms . mkStdGen $ seed
  where
    genIvs [] = []
    genIvs [_] = []
    genIvs (x:y:xs) = let lo = forceRange 1 max x
                          sz = forceRange 0 lap y
                      in (lo, lo + sz) : genIvs xs


cDATA_SIZE :: Int
cDATA_SIZE =  1000

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
      let ivs  = genRandomIntervals cDATA_SIZE 50 cDATA_SIZE
      let n = show cDATA_SIZE
      ivsP   <- ensure $ [IV lo hi | (lo,hi) <- ivs]
      cS     <- ensure $ C.fromList ivsP
      sS     <- ensure $ S.fromList ivsP
      oIvsP  <- ensure $ C.toAscList cS
      daIvsP <- ensure $ [IV x x | x <- [1 .. cDATA_SIZE]]
      mainWith
       (do
         func ("Data.Set    fromList " ++ n)            C.fromList ivsP
         func ("IntervalSet fromList " ++ n)            S.fromList ivsP
         func ("Data.Set    fromAscList " ++ n)         C.fromAscList oIvsP
         func ("IntervalSet fromAscList " ++ n)         S.fromAscList oIvsP
         func ("Data.Set    fromDistinctAscList " ++ n) C.fromDistinctAscList daIvsP
         func ("IntervalSet fromDistinctAscList " ++ n) S.fromDistinctAscList daIvsP
         func ("Data.Set    mapMonotonic " ++ n)        (C.mapMonotonic id) cS
         func ("IntervalSet mapMonotonic " ++ n)        (S.mapMonotonic id) sS
         )
