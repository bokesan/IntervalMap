{- |
Module      :  Data.IntervalMap
Copyright   :  (c) Christoph Breitkopf 2011
License     :  BSD-style
Maintainer  :  chbreitkopf@gmail.com
Stability   :  experimental
Portability :  portable

An implementation of maps from intervals to values. The key intervals may
overlap, and the implementation contains efficient search functions
for all keys containing a point or overlapping an interval.
Closed, open, and half-open intervals can be contained in the same map.

This module re-exports the value lazy "Data.IntervalMap.Lazy" API, plus
several value strict functions from "Data.IntervalMap.Strict".
-}
module Data.IntervalMap
    ( module Data.IntervalMap.Lazy
    , insertWith'
    , insertWithKey'
    , insertLookupWithKey'
    , fold
    , foldWithKey
    ) where

import Data.IntervalMap.Lazy
import qualified Data.IntervalMap.Lazy as L
import qualified Data.IntervalMap.Strict as S

-- | /Deprecated./ As of version 0.3, replaced by 'S.insertWith'.
--
-- /O(log n)/. Same as 'insertWith', but the combining function is
-- applied strictly.  This is often the most desirable behavior.
--
-- For example, to update a counter:
--
-- > insertWith' (+) k 1 m
--
insertWith' :: Ord k => (a -> a -> a) -> Interval k -> a -> IntervalMap k a -> IntervalMap k a
insertWith' = S.insertWith
{-# INLINABLE insertWith' #-}

-- | /Deprecated./ As of version 0.3, replaced by 'S.insertWithKey'.
--
-- /O(log n)/. Same as 'insertWithKey', but the combining function is
-- applied strictly.
insertWithKey' :: Ord k => (Interval k -> a -> a -> a) -> Interval k -> a -> IntervalMap k a -> IntervalMap k a
insertWithKey' = S.insertWithKey
{-# INLINABLE insertWithKey' #-}

-- | /Deprecated./ As of version 0.3, replaced by
-- 'S.insertLookupWithKey'.
--
-- /O(log n)/. A strict version of 'insertLookupWithKey'.
insertLookupWithKey' :: Ord k => (Interval k -> a -> a -> a) -> Interval k -> a -> IntervalMap k a
                     -> (Maybe a, IntervalMap k a)
insertLookupWithKey' = S.insertLookupWithKey

-- | /Deprecated./ As of version 0.5, replaced by 'L.foldr'.
--
-- /O(n)/. Fold the values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
fold :: (a -> b -> b) -> b -> IntervalMap k a -> b
fold = L.foldr

-- | /Deprecated./ As of version 0.3, replaced by 'L.foldrWithKey'.
--
-- /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldrWithKey' and is present
-- for compatibility only.
foldWithKey :: (Interval k -> a -> b -> b) -> b -> IntervalMap k a -> b
foldWithKey = foldrWithKey
