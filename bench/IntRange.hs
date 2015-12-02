{-# LANGUAGE MultiParamTypeClasses #-}

module IntRange (IntRange(..), Interval(..)) where

import Data.IntervalMap.Generic.Interval
import Control.DeepSeq

data IntRange = IntRange {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Eq, Ord, Show)

instance Interval IntRange Int where
  lowerBound (IntRange lo _) = lo
  upperBound (IntRange _ hi) = hi

instance NFData IntRange where
  rnf a = a `seq` ()
