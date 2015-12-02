module IvMapSortedList (IVS, size, empty, singleton, insert, insertWithKey, lookup, containing, fromList) where

import Prelude hiding (lookup)
import Data.IntervalMap.Generic.Interval
import Control.DeepSeq
import Data.List (sortBy)


data Entry k v = E !k !v deriving (Eq, Ord, Show)

instance (NFData k, NFData v) => NFData (Entry k v) where
  rnf (E k v) = k `deepseq` v `deepseq` ()

newtype IVS k v = IVS [Entry k v] deriving (Eq, Ord, Show)

instance (NFData k, NFData v) => NFData (IVS k v) where
  rnf (IVS v) = v `deepseq` ()

size :: IVS k v -> Int
size (IVS es) = length es

empty :: IVS k v
empty = IVS []

singleton :: k -> v -> IVS k v
singleton k v = IVS [E k v]

insert :: Ord k => k -> v -> IVS k v -> IVS k v
insert =  insertWithKey (\_ val _ -> val)

insertWithKey :: Ord k => (k -> v -> v -> v) -> k -> v -> IVS k v -> IVS k v
insertWithKey f key val (IVS m) = IVS (go m)
  where
    go [] = [E key val] 
    go es@(e@(E k v) : es') = case compare key k of
                                GT -> e : go es'
                                LT -> E key val : es
                                EQ -> E key (f key val v) : es'

lookup :: Ord k => k -> IVS k v -> Maybe v
lookup key (IVS m) = key `seq` go m
  where
    go (E k v : es) = case compare key k of
                        GT -> go es
                        EQ -> Just v
                        LT -> Nothing
    go [] = Nothing


containing :: (Interval k e) => IVS k v -> e -> [(k, v)]
(IVS m) `containing` p = p `seq` go m
  where
    go (E k v : es) | p `above` k = go es
                    | p `below` k = []
                    | otherwise   = (k,v) : go es
    go [] = []

fromList :: Ord k => [(k, v)] -> IVS k v
fromList =  foldr (\(k,v) m -> insert k v m) empty . sortBy cmpKey
  where cmpKey (k1,_) (k2,_) = compare k1 k2
