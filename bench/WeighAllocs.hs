{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Weigh

import Control.DeepSeq
import Prelude hiding (max)
import System.Random
import Data.Foldable (foldr)

import Data.IntervalMap.Generic.Interval
import qualified Data.IntervalSet as S
import qualified Data.Set as C
import qualified Data.Map.Strict as M
import qualified Data.IntervalMap.Generic.Strict as IVM


seed :: Int
seed = 54321

ensure :: NFData a => a -> IO a
ensure xs = xs `deepseq` return xs

forceRange :: Int -> Int -> Int -> Int
forceRange lo hi n | n >= lo && n <= hi = n
                   | n < 0              = forceRange lo hi (negate n)
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
      ivsP   <- ensure [IV lo hi | (lo,hi) <- ivs]
      cS     <- ensure $ C.fromList ivsP
      sS     <- ensure $ S.fromList ivsP
      oIvsP  <- ensure $ C.toAscList cS
      let m = show (length oIvsP)
      kvs    <- ensure [(iv, lowerBound iv) | iv <- ivsP]
      cMap   <- ensure $ M.fromList kvs
      ivMap  <- ensure $ IVM.fromList kvs
      oKvs   <- ensure $ M.toAscList cMap
      mainWith
       (do
         func ("Data.Set    fromList " ++ n)            C.fromList ivsP
         func ("IntervalSet fromList " ++ n)            S.fromList ivsP
         func ("Data.Set    fromAscList " ++ m)         C.fromAscList oIvsP
         func ("IntervalSet fromAscList " ++ m)         S.fromAscList oIvsP
         func ("Data.Set    fromDistinctAscList " ++ m) C.fromDistinctAscList oIvsP
         func ("IntervalSet fromDistinctAscList " ++ m) S.fromDistinctAscList oIvsP
         func ("Data.Set    mapMonotonic " ++ m)        (C.mapMonotonic id) cS
         func ("IntervalSet mapMonotonic " ++ m)        (S.mapMonotonic id) sS
         func ("Data.Map    fromList " ++ n)            M.fromList kvs
         func ("IntervalMap fromList " ++ n)            IVM.fromList kvs
         func ("Data.Map    fromAscList " ++ m)         M.fromAscList oKvs
         func ("IntervalMap fromAscList " ++ m)         IVM.fromAscList oKvs
         func ("Data.Map    fromDistinctAscList " ++ m) M.fromDistinctAscList oKvs
         func ("IntervalMap fromDistinctAscList " ++ m) IVM.fromDistinctAscList oKvs
         func ("Data.Map    mapKeysMonotonic " ++ m)    (M.mapKeysMonotonic id) cMap
         func ("IntervalMap mapKeysMonotonic " ++ m)    (IVM.mapKeysMonotonic id) ivMap
         )
