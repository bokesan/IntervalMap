-- |
-- Module      :  Data.IntervalMap.Generic.Base
-- Copyright   :  (c) Christoph Breitkopf 2014
-- License     :  BSD-style
-- Maintainer  :  chbreitkopf@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTC with FD)
--
-- An implementation of maps from intervals to values. The key intervals may
-- overlap, and the implementation contains efficient search functions
-- for all keys containing a point or overlapping an interval.
-- Closed, open, and half-open intervals can be contained in the same map.
--
-- An IntervalMap cannot contain duplicate keys - if you need to map a key
-- to multiple values, use a collection as the value type, for
-- example: @IntervalMap /k/ [/v/]@.
--
-- It is an error to insert an empty interval into a map. This precondition is not
-- checked by the various construction functions.
--
-- Since many function names (but not the type name) clash with
-- /Prelude/ names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.Generic.IntervalMap.Strict (IntervalMap)
-- >  import qualified Data.Generic.IntervalMap.Strict as IM
--
-- It offers most of the same functions as 'Data.Map', but the key type must be an
-- instance of 'Interval'.
-- Some functions differ in asymptotic performance (for example 'size') or have not
-- been tuned for efficiency as much as their equivalents in 'Data.Map' (in
-- particular the various set functions).
--
-- In addition, there are functions specific to maps of intervals, for example to search
-- for all keys containing a given point or contained in a given interval.
--
-- The implementation is a red-black tree augmented with the maximum upper bound
-- of all keys.
--
-- Parts of this implementation are based on code from the 'Data.Map' implementation,
-- (c) Daan Leijen 2002, (c) Andriy Palamarchuk 2008.
-- The red-black tree deletion is based on code from llrbtree by Kazu Yamamoto.
-- Of course, any errors are mine.
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.IntervalMap.Generic.Base (
            -- * re-export
            Interval(..)
            -- * Map type
            , IntervalMap(..)      -- instance Eq,Show,Read

            -- * Operators
            , (!), (\\)

            -- * Query
            , null
            , size
            , member
            , notMember
            , lookup
            , findWithDefault

            -- ** Interval query
            , containing
            , intersecting
            , within
            
            -- * Construction
            , empty
            , singleton

            -- ** Insertion
            , insert
            , insertWith
            , insertWithKey
            , insertLookupWithKey
            
            -- ** Delete\/Update
            , delete
            , adjust
            , adjustWithKey
            , update
            , updateWithKey
            , updateLookupWithKey
            , alter

            -- * Combine

            -- ** Union
            , union
            , unionWith
            , unionWithKey
            , unions
            , unionsWith

            -- ** Difference
            , difference
            , differenceWith
            , differenceWithKey
            
            -- ** Intersection
            , intersection
            , intersectionWith
            , intersectionWithKey

            -- * Traversal
            -- ** Map
            , map
            , mapWithKey
            , mapAccum
            , mapAccumWithKey
            , mapAccumRWithKey
            , mapKeys
            , mapKeysWith
            , mapKeysMonotonic

            -- ** Fold
            , foldr, foldl
            , foldrWithKey, foldlWithKey
            , foldl', foldr'
            , foldrWithKey', foldlWithKey'

            -- * Flatten
            , flattenWith, flattenWithMonotonic

            -- * Conversion
            , elems
            , keys
            , keysSet
            , assocs

            -- ** Lists
            , toList
            , fromList
            , fromListWith
            , fromListWithKey

            -- ** Ordered lists
            , toAscList
            , toDescList
            , fromAscList
            , fromAscListWith
            , fromAscListWithKey
            , fromDistinctAscList

            -- * Filter
            , filter
            , filterWithKey
            , partition
            , partitionWithKey

            , mapMaybe
            , mapMaybeWithKey
            , mapEither
            , mapEitherWithKey

            , split
            , splitLookup
            , splitAt
            , splitIntersecting

            -- * Submap
            , isSubmapOf, isSubmapOfBy
            , isProperSubmapOf, isProperSubmapOfBy

            -- * Min\/Max
            , findMin
            , findMax
            , findLast
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , updateMin
            , updateMax
            , updateMinWithKey
            , updateMaxWithKey
            , minView
            , maxView
            , minViewWithKey
            , maxViewWithKey

            -- * Internal, not re-exported by Data.IntervalMap.{Lazy,Strict}
            , Color(..)
            , balanceL, balanceR
            , turnBlack
            , setMinValue, setMaxValue

            -- * Debugging
            , valid

            -- * Testing
            , height, maxHeight, showStats

            ) where

import Prelude hiding (null, lookup, map, filter, foldr, foldl, splitAt)
import Data.Maybe (fromJust)
import Data.Bits (shiftR, (.&.))
import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(..), (<$>), (<|>))
import Data.Traversable (Traversable(traverse))
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import qualified Data.Set as Set
import Control.DeepSeq
import GHC.Generics

import Data.IntervalMap.Generic.Interval

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,\\ --

-- | /O(log n)/. Lookup value for given key. Calls 'error' if the key is not in the map.
--
-- Use 'lookup' or 'findWithDefault' instead of this function, unless you are absolutely
-- sure that the key is present in the map.
(!) :: (Interval k e, Ord k) => IntervalMap k v -> k -> v
tree ! key = case lookup key tree of
               Just v  -> v
               Nothing -> error "IntervalMap.!: key not found"

-- | Same as 'difference'.
(\\) :: (Interval k e, Ord k) => IntervalMap k a -> IntervalMap k b -> IntervalMap k a
m1 \\ m2 = difference m1 m2


data Color = R | B deriving (Eq, Read, Show, Generic)

-- | A map from intervals of type @k@ to values of type @v@.
data IntervalMap k v = Nil
                      | Node !Color
                             !k -- key
                             !k -- interval with maximum upper in tree
                             v             -- value
                             !(IntervalMap k v) -- left subtree
                             !(IntervalMap k v) -- right subtree
  deriving (Generic)
instance (Eq k, Eq v) => Eq (IntervalMap k v) where
  a == b = toAscList a == toAscList b

instance (Ord k, Ord v) => Ord (IntervalMap k v) where
  compare a b = compare (toAscList a) (toAscList b)

instance Functor (IntervalMap k) where
  fmap f m  = map f m

instance (Interval i k, Ord i) => Monoid (IntervalMap i v) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance Traversable (IntervalMap k) where
  traverse _ Nil = pure Nil
  traverse f (Node c k m v l r)
    = flip (Node c k m) <$> traverse f l <*> f v <*> traverse f r

instance Foldable.Foldable (IntervalMap k) where
  fold Nil = mempty
  fold (Node _ _ _ v l r) = Foldable.fold l `mappend` v `mappend` Foldable.fold r
  foldr = foldr
  foldl = foldl
  foldMap _ Nil = mempty
  foldMap f (Node _ _ _ v l r) = Foldable.foldMap f l `mappend` f v `mappend` Foldable.foldMap f r

instance (NFData k, NFData a) => NFData (IntervalMap k a) where
    rnf Nil = ()
    rnf (Node _ kx _ x l r) = kx `deepseq` x `deepseq` l `deepseq` r `deepseq` ()

instance (Read e, Interval i k, Ord i, Read i) => Read (IntervalMap i e) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)

instance (Show k, Show a) => Show (IntervalMap k a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)


isRed :: IntervalMap k v -> Bool
isRed (Node R _ _ _ _ _) = True
isRed _ = False

turnBlack :: IntervalMap k v -> IntervalMap k v
turnBlack (Node R k m vs l r) = Node B k m vs l r
turnBlack t = t

turnRed :: IntervalMap k v -> IntervalMap k v
turnRed Nil = error "turnRed: Leaf"
turnRed (Node B k m v l r) = Node R k m v l r
turnRed t = t

-- construct node, recomputing the upper key bound.
mNode :: (Interval k e) => Color -> k -> v -> IntervalMap k v -> IntervalMap k v -> IntervalMap k v
mNode c k v l r = Node c k (maxUpper k l r) v l r

maxUpper :: (Interval i k) => i -> IntervalMap i v -> IntervalMap i v -> i
maxUpper k Nil                Nil                = k `seq` k
maxUpper k Nil                (Node _ _ m _ _ _) = maxByUpper k m
maxUpper k (Node _ _ m _ _ _) Nil                = maxByUpper k m
maxUpper k (Node _ _ l _ _ _) (Node _ _ r _ _ _) = maxByUpper k (maxByUpper l r)

-- interval with the greatest upper bound. The lower bound is ignored!
maxByUpper :: (Interval i e) => i -> i -> i
maxByUpper a b = a `seq` b `seq`
                 case compareUpperBounds a b of
                   LT -> b
                   _  -> a

-- ---------------------------------------------------------

-- | /O(1)/. The empty map.
empty :: IntervalMap k v
empty =  Nil

-- | /O(1)/. A map with one entry.
singleton :: k -> v -> IntervalMap k v
singleton k v = Node B k k v Nil Nil


-- | /O(1)/. Is the map empty?
null :: IntervalMap k v -> Bool
null Nil = True
null _   = False

-- | /O(n)/. Number of keys in the map.
--
-- Caution: unlike 'Data.Map.size', which takes constant time, this is linear in the
-- number of keys!
size :: IntervalMap k v -> Int
size t = h 0 t
  where
    h n m = n `seq` case m of
                      Nil -> n
                      Node _ _ _ _ l r -> h (h n l + 1) r

-- | The height of the tree. For testing/debugging only.
height :: IntervalMap k v -> Int
height Nil = 0
height (Node _ _ _ _ l r) = 1 + max (height l) (height r)

-- | The maximum height of a red-black tree with the given number of nodes.
-- For testing/debugging only.
maxHeight :: Int -> Int
maxHeight nodes = 2 * log2 (nodes + 1)

-- | Tree statistics (size, height, maxHeight size).
-- For testing/debugging only.
showStats :: IntervalMap k a -> (Int, Int, Int)
showStats m = (n, height m, maxHeight n)
  where n = size m

-- | /O(log n)/. Does the map contain the given key? See also 'notMember'.
member :: (Ord k) => k -> IntervalMap k v -> Bool
member key tree = case lookup key tree of
                    Nothing -> False
                    Just _  -> True

-- | /O(log n)/. Does the map not contain the given key? See also 'member'.
notMember :: (Ord k) => k -> IntervalMap k v -> Bool
notMember key tree = not (member key tree)


-- | /O(log n)/. Look up the given key in the map, returning the value @('Just' value)@,
-- or 'Nothing' if the key is not in the map.
lookup :: (Ord k) => k -> IntervalMap k v -> Maybe v
lookup k Nil =  k `seq` Nothing
lookup k (Node _ key _ v l r) = case compare k key of
                                  LT -> lookup k l
                                  GT -> lookup k r
                                  EQ -> Just v


-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
findWithDefault :: Ord k => a -> k -> IntervalMap k a -> a
findWithDefault def k m = case lookup k m of
    Nothing -> def
    Just x  -> x

-- | Return the submap of key intervals containing the given point.
-- This is the second element of the value of 'splitAt':
--
-- > m `containing` p == let (_,m',_) = m `splitAt` p in m'
--
-- /O(n)/, since potentially all keys could contain the point.
-- /O(log n)/ average case. This is also the worst case for maps containing no overlapping keys.
containing :: (Interval k e) => IntervalMap k v -> e -> IntervalMap k v
t `containing` pt = pt `seq` fromDistinctAscList (go [] pt t)
  where
    go xs _ Nil = xs
    go xs p (Node _ k m v l r)
       | p `above` m  =  xs         -- above all intervals in the tree: no result
       | p `below` k  =  go xs p l  -- to the left of the lower bound: can't be in right subtree
       | p `inside` k =  go ((k,v) : go xs p r) p l
       | otherwise    =  go (go xs p r) p l

-- | Return the submap of key intervals overlapping (intersecting) the given interval.
-- This is the second element of the value of 'splitIntersecting':
--
-- > m `intersecting` i == let (_,m',_) = m `splitIntersecting` i in m'
--
-- /O(n)/, since potentially all keys could intersect the interval.
-- /O(log n)/ average case, if few keys intersect the interval.
intersecting :: (Interval k e) => IntervalMap k v -> k -> IntervalMap k v
t `intersecting` iv = iv `seq` fromDistinctAscList (go [] iv t)
  where
    go xs _ Nil = xs
    go xs i (Node _ k m v l r)
       | i `after` m     =  xs
       | i `before` k    =  go xs i l
       | i `overlaps` k  =  go ((k,v) : go xs i r) i l
       | otherwise       =  go (go xs i r) i l

-- | Return the submap of key intervals completely inside the given interval.
--
-- /O(n)/, since potentially all keys could be inside the interval.
-- /O(log n)/ average case, if few keys are inside the interval.
within :: (Interval k e) => IntervalMap k v -> k -> IntervalMap k v
t `within` iv = iv `seq` fromDistinctAscList (go [] iv t)
  where
    go xs _ Nil = xs
    go xs i (Node _ k m v l r)
       | i `after` m     =  xs
       | i `before` k    =  go xs i l
       | i `subsumes` k  =  go ((k,v) : go xs i r) i l
       | otherwise       =  go (go xs i r) i l


-- | /O(log n)/. Insert a new key/value pair. If the map already contains the key, its value is
-- changed to the new value.
insert :: (Interval k e, Ord k) => k -> v -> IntervalMap k v -> IntervalMap k v
insert =  insertWithKey (\_ v _ -> v)

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@ 
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
insertWith :: (Interval k e, Ord k) => (v -> v -> v) -> k -> v -> IntervalMap k v -> IntervalMap k v
insertWith f = insertWithKey (\_ new old -> f new old)

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@ 
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
insertWithKey :: (Interval k e, Ord k) => (k -> v -> v -> v) -> k -> v -> IntervalMap k v -> IntervalMap k v
insertWithKey f key value mp  =  key `seq` turnBlack (ins mp)
  where
    singletonR k v = Node R k k v Nil Nil
    ins Nil = singletonR key value
    ins (Node color k m v l r) =
      case compare key k of
        LT -> balanceL color k v (ins l) r
        GT -> balanceR color k v l (ins r)
        EQ -> Node color k m (f k value v) l r

-- | /O(log n)/. Combine insert with old values retrieval.
insertLookupWithKey :: (Interval k e, Ord k) => (k -> v -> v -> v) -> k -> v -> IntervalMap k v -> (Maybe v, IntervalMap k v)
insertLookupWithKey f key value mp  =  key `seq` (oldval, turnBlack mp')
  where
    (oldval, mp') = ins mp
    singletonR k v = Node R k k v Nil Nil
    ins Nil = (Nothing, singletonR key value)
    ins (Node color k m v l r) =
      case compare key k of
        LT -> case ins l of
                 (x@(Just _), t') -> (x, Node color k m v t' r)
                 (Nothing, t') -> (Nothing, balanceL color k v t' r)
        GT -> case ins r of
                 (x@(Just _), t') -> (x, Node color k m v l t')
                 (Nothing, t') -> (Nothing, balanceR color k v l t')
        EQ -> (Just v, Node color k m (f k value v) l r)


balanceL :: (Interval k e) => Color -> k -> v -> IntervalMap k v -> IntervalMap k v -> IntervalMap k v
balanceL B zk zv (Node R yk _ yv (Node R xk _ xv a b) c) d =
    mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceL B zk zv (Node R xk _ xv a (Node R yk _ yv b c)) d =
    mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceL c xk xv l r = mNode c xk xv l r

balanceR :: (Interval k e) => Color -> k -> v -> IntervalMap k v -> IntervalMap k v -> IntervalMap k v
balanceR B xk xv a (Node R yk _ yv b (Node R zk _ zv c d)) =
    mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceR B xk xv a (Node R zk _ zv (Node R yk _ yv b c) d) =
    mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceR c xk xv l r = mNode c xk xv l r


-- min/max

-- | /O(log n)/. Returns the smallest key and its associated value.
-- Calls 'error' if the map is empty.
findMin :: IntervalMap k v -> (k, v)
findMin (Node _ k _ v Nil _) = (k,v)
findMin (Node _ _ _ _ l _) = findMin l
findMin Nil = error "IntervalMap.findMin: empty map"

-- | /O(log n)/. Returns the largest key and its associated value.
-- Calls 'error' if the map is empty.
findMax :: IntervalMap k v -> (k, v)
findMax (Node _ k _ v _ Nil) = (k,v)
findMax (Node _ _ _ _ _ r) = findMax r
findMax Nil = error "IntervalMap.findMin: empty map"

-- | Returns the key with the largest endpoint and its associated value.
-- If there is more than one key with that endpoint, return the rightmost.
--
-- /O(n)/, since all keys could have the same endpoint.
-- /O(log n)/ average case.
findLast :: (Interval k e) => IntervalMap k v -> (k, v)
findLast Nil = error "IntervalMap.findLast: empty map"
findLast t@(Node _ _ mx _ _ _) = fromJust (go t)
  where
    go Nil = Nothing
    go (Node _ k m v l r) | sameU m mx = if sameU k m then go r <|> Just (k,v)
                                                      else go r <|> go l
                          | otherwise  = Nothing
    sameU a b = compareUpperBounds a b == EQ


-- Type to indicate whether the number of black nodes changed or stayed the same.
data DeleteResult k v = U !(IntervalMap k v)   -- Unchanged
                      | S !(IntervalMap k v)   -- Shrunk

unwrap :: DeleteResult k v -> IntervalMap k v
unwrap (U m) = m
unwrap (S m) = m

-- DeleteResult with value
data DeleteResult' k v a = U' !(IntervalMap k v) a
                         | S' !(IntervalMap k v) a

unwrap' :: DeleteResult' k v a -> IntervalMap k v
unwrap' (U' m _) = m
unwrap' (S' m _) = m

-- annotate DeleteResult with value
annotate :: DeleteResult k v -> a -> DeleteResult' k v a
annotate (U m) x = U' m x
annotate (S m) x = S' m x


-- | /O(log n)/. Remove the smallest key from the map. Return the empty map if the map is empty.
deleteMin :: (Interval k e, Ord k) => IntervalMap k v -> IntervalMap k v
deleteMin Nil = Nil
deleteMin m   = turnBlack (unwrap' (deleteMin' m))

deleteMin' :: (Interval k e, Ord k) => IntervalMap k v -> DeleteResult' k v (k, v)
deleteMin' Nil = error "deleteMin': Nil"
deleteMin' (Node B k _ v Nil Nil) = S' Nil (k,v)
deleteMin' (Node B k _ v Nil r@(Node R _ _ _ _ _)) = U' (turnBlack r) (k,v)
deleteMin' (Node R k _ v Nil r) = U' r (k,v)
deleteMin' (Node c k _ v l r) =
  case deleteMin' l of
    (U' l' kv) -> U' (mNode c k v l' r) kv
    (S' l' kv) -> annotate (unbalancedR c k v l' r) kv

deleteMax' :: (Interval k e, Ord k) => IntervalMap k v -> DeleteResult' k v (k, v)
deleteMax' Nil = error "deleteMax': Nil"
deleteMax' (Node B k _ v Nil Nil) = S' Nil (k,v)
deleteMax' (Node B k _ v l@(Node R _ _ _ _ _) Nil) = U' (turnBlack l) (k,v)
deleteMax' (Node R k _ v l Nil) = U' l (k,v)
deleteMax' (Node c k _ v l r) =
  case deleteMax' r of
    (U' r' kv) -> U' (mNode c k v l r') kv
    (S' r' kv) -> annotate (unbalancedL c k v l r') kv

-- The left tree lacks one Black node
unbalancedR :: (Interval k e, Ord k) => Color -> k -> v -> IntervalMap k v -> IntervalMap k v -> DeleteResult k v
-- Decreasing one Black node in the right
unbalancedR B k v l r@(Node B _ _ _ _ _) = S (balanceR B k v l (turnRed r))
unbalancedR R k v l r@(Node B _ _ _ _ _) = U (balanceR B k v l (turnRed r))
-- Taking one Red node from the right and adding it to the right as Black
unbalancedR B k v l (Node R rk _ rv rl@(Node B _ _ _ _ _) rr)
  = U (mNode B rk rv (balanceR B k v l (turnRed rl)) rr)
unbalancedR _ _ _ _ _ = error "unbalancedR"

unbalancedL :: (Interval k e, Ord k) => Color -> k -> v -> IntervalMap k v -> IntervalMap k v -> DeleteResult k v
unbalancedL R k v l@(Node B _ _ _ _ _) r = U (balanceL B k v (turnRed l) r)
unbalancedL B k v l@(Node B _ _ _ _ _) r = S (balanceL B k v (turnRed l) r)
unbalancedL B k v (Node R lk _ lv ll lr@(Node B _ _ _ _ _)) r
  = U (mNode B lk lv ll (balanceL B k v (turnRed lr) r))
unbalancedL _ _ _ _ _ = error "unbalancedL"



-- | /O(log n)/. Remove the largest key from the map. Return the empty map if the map is empty.
deleteMax :: (Interval k e, Ord k) => IntervalMap k v -> IntervalMap k v
deleteMax Nil = Nil
deleteMax m   = turnBlack (unwrap' (deleteMax' m))

-- | /O(log n)/. Delete and return the smallest key.
deleteFindMin :: (Interval k e, Ord k) => IntervalMap k v -> ((k,v), IntervalMap k v)
deleteFindMin mp = case deleteMin' mp of
                     (U' r v) -> (v, turnBlack r)
                     (S' r v) -> (v, turnBlack r)

-- | /O(log n)/. Delete and return the largest key.
deleteFindMax :: (Interval k e, Ord k) => IntervalMap k v -> ((k,v), IntervalMap k v)
deleteFindMax mp = case deleteMax' mp of
                     (U' r v) -> (v, turnBlack r)
                     (S' r v) -> (v, turnBlack r)

-- | /O(log n)/. Update or delete value at minimum key.
updateMin :: (Interval k e, Ord k) => (v -> Maybe v) -> IntervalMap k v -> IntervalMap k v
updateMin f m = updateMinWithKey (\_ v -> f v) m

-- | /O(log n)/. Update or delete value at maximum key.
updateMax :: (Interval k e, Ord k) => (v -> Maybe v) -> IntervalMap k v -> IntervalMap k v
updateMax f m = updateMaxWithKey (\_ v -> f v) m

-- | /O(log n)/. Update or delete value at minimum key.
updateMinWithKey :: (Interval k e, Ord k) => (k -> v -> Maybe v) -> IntervalMap k v -> IntervalMap k v
updateMinWithKey _ Nil = Nil
updateMinWithKey f m = let (k,v) = findMin m in
                       case f k v of
                         Just v' -> setMinValue v' m
                         Nothing -> deleteMin m

-- | /O(log n)/. Update or delete value at maximum key.
updateMaxWithKey :: (Interval k e, Ord k) => (k -> v -> Maybe v) -> IntervalMap k v -> IntervalMap k v
updateMaxWithKey _ Nil = Nil
updateMaxWithKey f m = let (k,v) = findMax m in
                       case f k v of
                         Just v' -> setMaxValue v' m
                         Nothing -> deleteMax m

-- | /O(log n)/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [([5,6],"a"), ([3,4],"b")]) == Just (([3,4],"b"), singleton [5,6] "a")
-- > minViewWithKey empty == Nothing

minViewWithKey :: (Interval k e, Ord k) => IntervalMap k a -> Maybe ((k, a), IntervalMap k a)
minViewWithKey Nil = Nothing
minViewWithKey x   = Just (deleteFindMin x)

-- | /O(log n)/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
maxViewWithKey :: (Interval k e, Ord k) => IntervalMap k a -> Maybe ((k, a), IntervalMap k a)
maxViewWithKey Nil = Nothing
maxViewWithKey x   = Just (deleteFindMax x)

-- | /O(log n)/. Retrieves the value associated with minimal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
minView :: (Interval k e, Ord k) => IntervalMap k a -> Maybe (a, IntervalMap k a)
minView Nil = Nothing
minView x   = case deleteFindMin x of ((_,a), x') -> Just (a, x')

-- | /O(log n)/. Retrieves the value associated with maximal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
maxView :: (Interval k e, Ord k) => IntervalMap k a -> Maybe (a, IntervalMap k a)
maxView Nil = Nothing
maxView x   = case deleteFindMax x of ((_,a), x') -> Just (a, x')


setMinValue :: v -> IntervalMap k v -> IntervalMap k v
setMinValue _  Nil = Nil
setMinValue v' (Node c k m _ Nil r) = Node c k m v' Nil r
setMinValue v' (Node c k m v l   r) = Node c k m v (setMinValue v' l) r

setMaxValue :: v -> IntervalMap k v -> IntervalMap k v
setMaxValue _  Nil = Nil
setMaxValue v' (Node c k m _ l Nil) = Node c k m v' l Nil
setMaxValue v' (Node c k m v l r)   = Node c k m v l (setMaxValue v' r)



-- folding

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
foldr :: (a -> b -> b) -> b -> IntervalMap k a -> b
foldr _ z Nil = z
foldr f z (Node _ _ _ x l r) = foldr f (f x (foldr f z r)) l

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> IntervalMap k a -> b
foldr' f z m = z `seq` case m of
                         Nil -> z
                         Node _ _ _ x l r -> foldr' f (f x (foldr' f z r)) l

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
foldl :: (b -> a -> b) -> b -> IntervalMap k a -> b
foldl _ z Nil = z
foldl f z (Node _ _ _ x l r) = foldl f (f (foldl f z l) x) r

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (b -> a -> b) -> b -> IntervalMap k a -> b
foldl' f z m = z `seq` case m of
                         Nil -> z
                         Node _ _ _ x l r -> foldl' f (f (foldl' f z l) x) r

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
foldrWithKey :: (k -> v -> a -> a) -> a -> IntervalMap k v -> a
foldrWithKey _ z Nil = z
foldrWithKey f z (Node _ k _ x l r) = foldrWithKey f (f k x (foldrWithKey f z r)) l

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (k -> v -> a -> a) -> a -> IntervalMap k v -> a
foldrWithKey' f z m = z `seq` case m of
                                Nil -> z
                                Node _ k _ x l r -> foldrWithKey' f (f k x (foldrWithKey' f z r)) l

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
foldlWithKey :: (a -> k -> v -> a) -> a -> IntervalMap k v -> a
foldlWithKey _ z Nil = z
foldlWithKey f z (Node _ k _ x l r) = foldlWithKey f (f (foldlWithKey f z l) k x) r

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> k -> v -> a) -> a -> IntervalMap k v -> a
foldlWithKey' f z m = z `seq` case m of
                                Nil -> z
                                Node _ k _ x l r -> foldlWithKey' f (f (foldlWithKey' f z l) k x) r

-- | /O(n log n)/. Build a new map by combining successive key/value pairs.
flattenWith :: (Ord k, Interval k e) => ((k,v) -> (k,v) -> Maybe (k,v)) -> IntervalMap k v -> IntervalMap k v
flattenWith combine m = fromList (combineSuccessive combine m)

-- | /O(n)/. Build a new map by combining successive key/value pairs.
-- Same as 'flattenWith', but works only when the combining functions returns
-- strictly monotonic key values.
flattenWithMonotonic :: (Interval k e) => ((k,v) -> (k,v) -> Maybe (k,v)) -> IntervalMap k v -> IntervalMap k v
flattenWithMonotonic combine m = fromDistinctAscList (combineSuccessive combine m)

combineSuccessive :: ((k,v) -> (k,v) -> Maybe (k,v)) -> IntervalMap k v -> [(k,v)]
combineSuccessive combine m = go (toAscList m)
  where
    go (x : xs@(y:ys)) = case combine x y of
                           Nothing -> x : go xs
                           Just x' -> go (x' : ys)
    go xs = xs


-- delete

-- | /O(log n)/. Delete a key from the map. If the map does not contain the key,
-- it is returned unchanged.
delete :: (Interval k e, Ord k) => k -> IntervalMap k v -> IntervalMap k v
delete key mp = turnBlack (unwrap (delete' key mp))

delete' :: (Interval k e, Ord k) => k -> IntervalMap k v -> DeleteResult k v
delete' x Nil = x `seq` U Nil
delete' x (Node c k _ v l r) =
  case compare x k of
    LT -> case delete' x l of
            (U l') -> U (mNode c k v l' r)
            (S l')    -> unbalancedR c k v l' r
    GT -> case delete' x r of
            (U r') -> U (mNode c k v l r')
            (S r')    -> unbalancedL c k v l r'
    EQ -> case r of
            Nil -> if c == B then blackify l else U l
            _ -> case deleteMin' r of
                   (U' r' (rk,rv)) -> U (mNode c rk rv l r')
                   (S' r' (rk,rv)) -> unbalancedL c rk rv l r'

blackify :: IntervalMap k v -> DeleteResult k v
blackify (Node R k m v l r) = U (Node B k m v l r)
blackify s                  = S s

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
adjust :: Ord k => (a -> a) -> k -> IntervalMap k a -> IntervalMap k a
adjust f k m = adjustWithKey (\_ v -> f v) k m

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
adjustWithKey :: Ord k => (k -> a -> a) -> k -> IntervalMap k a -> IntervalMap k a
adjustWithKey _ _ Nil = Nil
adjustWithKey f x (Node c k m v l r) =
  case compare x k of
    LT -> Node c k m v (adjustWithKey f x l) r
    GT -> Node c k m v l (adjustWithKey f x r)
    EQ -> Node c k m (f k v) l r

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: (Interval k e, Ord k) => (a -> Maybe a) -> k -> IntervalMap k a -> IntervalMap k a
update f k m = updateWithKey (\_ v -> f v) k m

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
updateWithKey :: (Interval k e, Ord k) => (k -> a -> Maybe a) -> k -> IntervalMap k a -> IntervalMap k a
updateWithKey f k m = snd (updateLookupWithKey f k m)

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
updateLookupWithKey :: (Interval k e, Ord k) => (k -> a -> Maybe a) -> k -> IntervalMap k a -> (Maybe a, IntervalMap k a)
updateLookupWithKey f x m = case lookup x m of
                              Nothing -> (Nothing, m)
                              r@(Just v) -> case f x v of
                                              Nothing -> (r, delete x m)
                                              r'@(Just v') -> (r', adjust (const v') x m)

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Interval k e, Ord k) => (Maybe a -> Maybe a) -> k -> IntervalMap k a -> IntervalMap k a
alter f x m = case lookup x m of
                Nothing -> case f Nothing of
                             Nothing -> m
                             Just v -> insert x v m
                y       -> case f y of
                             Nothing -> delete x m
                             Just v' -> adjust (const v') x m


-- | /O(n+m)/. The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@. 
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
union :: (Interval k e, Ord k) => IntervalMap k a -> IntervalMap k a -> IntervalMap k a
union m1 m2 = unionWithKey (\_ v _ -> v) m1 m2

-- | /O(n+m)/. Union with a combining function.
unionWith :: (Interval k e, Ord k) => (a -> a -> a) -> IntervalMap k a -> IntervalMap k a -> IntervalMap k a
unionWith f m1 m2 = unionWithKey (\_ v1 v2 -> f v1 v2) m1 m2

-- | /O(n+m)/. Union with a combining function.
unionWithKey :: (Interval k e, Ord k) => (k -> a -> a -> a) -> IntervalMap k a -> IntervalMap k a -> IntervalMap k a
unionWithKey f m1 m2 = fromDistinctAscList (ascListUnion f (toAscList m1) (toAscList m2))

-- | The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
unions :: (Interval k e, Ord k) => [IntervalMap k a] -> IntervalMap k a
unions ms = unionsWith const ms

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
unionsWith :: (Interval k e, Ord k) => (a -> a -> a) -> [IntervalMap k a] -> IntervalMap k a
unionsWith _ []  = empty
unionsWith _ [m] = m
unionsWith f ms = fromDistinctAscList (head (go (L.map toAscList ms)))
  where
    f' _ l r = f l r
    merge m1 m2 = ascListUnion f' m1 m2
    go [] = []
    go xs@[_] = xs
    go (x:y:xs) = go (merge x y : go xs)

-- | /O(n+m)/. Difference of two maps. 
-- Return elements of the first map not existing in the second map.
difference :: (Interval k e, Ord k) => IntervalMap k a -> IntervalMap k b -> IntervalMap k a
difference m1 m2 = differenceWithKey (\_ _ _ -> Nothing) m1 m2

-- | /O(n+m)/. Difference with a combining function. 
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@. 
differenceWith :: (Interval k e, Ord k) => (a -> b -> Maybe a) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k a
differenceWith f m1 m2 = differenceWithKey (\_ v1 v2 -> f v1 v2) m1 m2

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@. 
differenceWithKey :: (Interval k e, Ord k) => (k -> a -> b -> Maybe a) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k a
differenceWithKey f m1 m2 = fromDistinctAscList (ascListDifference f (toAscList m1) (toAscList m2))

-- | /O(n+m)/. Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
intersection :: (Interval k e, Ord k) => IntervalMap k a -> IntervalMap k b -> IntervalMap k a
intersection m1 m2 = intersectionWithKey (\_ v _ -> v) m1 m2

-- | /O(n+m)/. Intersection with a combining function.
intersectionWith :: (Interval k e, Ord k) => (a -> b -> c) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k c
intersectionWith f m1 m2 = intersectionWithKey (\_ v1 v2 -> f v1 v2) m1 m2

-- | /O(n+m)/. Intersection with a combining function.
intersectionWithKey :: (Interval k e, Ord k) => (k -> a -> b -> c) -> IntervalMap k a -> IntervalMap k b -> IntervalMap k c
intersectionWithKey f m1 m2 = fromDistinctAscList (ascListIntersection f (toAscList m1) (toAscList m2))

ascListUnion :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> [(k,a)] -> [(k,a)]
ascListUnion _ [] [] = []
ascListUnion _ [] ys = ys
ascListUnion _ xs [] = xs
ascListUnion f xs@(x@(xk,xv):xs') ys@(y@(yk,yv):ys') =
  case compare xk yk of
    LT -> x : ascListUnion f xs' ys
    GT -> y : ascListUnion f xs ys'
    EQ -> (xk, f xk xv yv) : ascListUnion f xs' ys'

ascListDifference :: Ord k => (k -> a -> b -> Maybe a) -> [(k,a)] -> [(k,b)] -> [(k,a)]
ascListDifference _ [] _  = []
ascListDifference _ xs [] = xs
ascListDifference f xs@(x@(xk,xv):xs') ys@((yk,yv):ys') =
  case compare xk yk of
    LT -> x : ascListDifference f xs' ys
    GT -> ascListDifference f xs ys'
    EQ -> case f xk xv yv of
            Nothing -> ascListDifference f xs' ys'
            Just v' -> (xk,v') : ascListDifference f xs' ys'

ascListIntersection :: Ord k => (k -> a -> b -> c) -> [(k,a)] -> [(k,b)] -> [(k,c)]
ascListIntersection _ [] _ = []
ascListIntersection _ _ [] = []
ascListIntersection f xs@((xk,xv):xs') ys@((yk,yv):ys') =
  case compare xk yk of
    LT -> ascListIntersection f xs' ys
    GT -> ascListIntersection f xs ys'
    EQ -> (xk, f xk xv yv) : ascListIntersection f xs' ys'


-- --- Conversion ---

-- | /O(n)/. The list of all key\/value pairs contained in the map, in ascending order of keys.
toAscList :: IntervalMap k v -> [(k,v)]
toAscList m = foldrWithKey (\k v r -> (k,v) : r) [] m

toAscList' :: IntervalMap k v -> [(k,v)] -> [(k,v)]
toAscList' m xs = foldrWithKey (\k v r -> (k,v) : r) xs m

-- | /O(n)/. The list of all key\/value pairs contained in the map, in no particular order.
toList :: IntervalMap k v -> [(k,v)]
toList m = toAscList m

-- | /O(n)/. The list of all key\/value pairs contained in the map, in descending order of keys.
toDescList :: IntervalMap k v -> [(k, v)]
toDescList m = foldlWithKey (\r k v -> (k,v) : r) [] m

-- | /O(n log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
fromList :: (Interval k e, Ord k) => [(k,v)] -> IntervalMap k v
fromList xs = L.foldl' (\m (k,v) -> insert k v m) empty xs

-- | /O(n log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
fromListWith :: (Interval k e, Ord k) => (a -> a -> a) -> [(k,a)] -> IntervalMap k a 
fromListWith f xs = fromListWithKey (\_ x y -> f x y) xs

-- | /O(n log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
fromListWithKey :: (Interval k e, Ord k) => (k -> a -> a -> a) -> [(k,a)] -> IntervalMap k a 
fromListWithKey f xs = L.foldl' ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t

-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: (Interval k e, Eq k) => [(k,v)] -> IntervalMap k v
fromAscList xs = fromAscListWith (\_ b -> b) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
fromAscListWith :: (Interval k e, Eq k) => (a -> a -> a) -> [(k,a)] -> IntervalMap k a 
fromAscListWith f xs = fromAscListWithKey (\_ a b -> f a b) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
fromAscListWithKey :: (Interval k e, Eq k) => (k -> a -> a -> a) -> [(k,a)] -> IntervalMap k a 
fromAscListWithKey f xs = fromDistinctAscList (combineEq f xs)

combineEq :: Eq k => (k -> a -> a -> a) -> [(k,a)] -> [(k,a)]
combineEq _ [] = []
combineEq _ xs@[_] = xs
combineEq f (x@(xk,xv) : xs@((yk,yv) : xs'))
  | xk == yk  = combineEq f ((xk, f xk xv yv) : xs')
  | otherwise = x : combineEq f xs


-- Strict tuple
data T2 a b = T2 !a !b


-- | /O(n)/. Build a map from an ascending list of elements with distinct keys in linear time.
-- /The precondition is not checked./
fromDistinctAscList :: (Interval k e) => [(k,v)] -> IntervalMap k v
-- exactly 2^n-1 items have height n. They can be all black
-- from 2^n - 2^n-2 items have height n+1. The lowest "row" should be red.
fromDistinctAscList lyst = case h (length lyst) lyst of
                             (T2 result []) -> result
                             _ -> error "fromDistinctAscList: list not fully consumed"
  where
    h n xs | n == 0      = T2 Nil xs
           | isPerfect n = buildB n xs
           | otherwise   = buildR n (log2 n) xs

    buildB n xs | xs `seq` n <= 0 = error "fromDictinctAscList: buildB 0"
                | n == 1     = case xs of ((k,v):xs') -> T2 (Node B k k v Nil Nil) xs'
                                          _ -> error "fromDictinctAscList: buildB 1"
                | otherwise  =
                     case n `quot` 2 of { n' ->
                     case buildB n' xs of { (T2 _ []) -> error "fromDictinctAscList: buildB n";
                                            (T2 l ((k,v):xs')) ->
                     case buildB n' xs' of { (T2 r xs'') ->
                     T2 (mNode B k v l r) xs'' }}}

    buildR n d xs | d `seq` xs `seq` n == 0 = T2 Nil xs
                  | n == 1    = case xs of ((k,v):xs') -> T2 (Node (if d==0 then R else B) k k v Nil Nil) xs'
                                           _ -> error "fromDistinctAscList: buildR 1"
                  | otherwise =
                      case n `quot` 2 of { n' ->
                      case buildR n' (d-1) xs of { (T2 _ []) -> error "fromDistinctAscList: buildR n";
                                                   (T2 l ((k,v):xs')) ->
                      case buildR (n - (n' + 1)) (d-1) xs' of { (T2 r xs'') ->
                      T2 (mNode B k v l r) xs'' }}}


-- is n a perfect binary tree size (2^m-1)?
isPerfect :: Int -> Bool
isPerfect n = (n .&. (n + 1)) == 0

log2 :: Int -> Int
log2 m = h (-1) m
  where
    h r n | r `seq` n <= 0 = r
          | otherwise      = h (r + 1) (n `shiftR` 1)


-- | /O(n)/. List of all values in the map, in ascending order of their keys.
elems :: IntervalMap k v -> [v]
elems m = [v | (_,v) <- toAscList m]

-- | /O(n)/. List of all keys in the map, in ascending order.
keys :: IntervalMap k v -> [k]
keys m = [k | (k,_) <- toAscList m]

-- | /O(n)/. Set of the keys.
keysSet :: IntervalMap k v -> Set.Set k
keysSet m =  Set.fromDistinctAscList (keys m)

-- | Same as 'toAscList'.
assocs :: IntervalMap k v -> [(k, v)]
assocs m = toAscList m

-- --- Mapping ---

-- | /O(n)/. Map a function over all values in the map.
map :: (a -> b) -> IntervalMap k a -> IntervalMap k b
map f = mapWithKey (\_ x -> f x)

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (k -> a -> b) -> IntervalMap k a -> IntervalMap k b
mapWithKey f = go
  where
    go Nil = Nil
    go (Node c k m v l r) = Node c k m (f k v) (go l) (go r)

-- | /O(n)/. The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
mapAccum :: (a -> b -> (a,c)) -> a -> IntervalMap k b -> (a, IntervalMap k c)
mapAccum f a m = mapAccumWithKey (\a' _ x' -> f a' x') a m

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumWithKey :: (a -> k -> b -> (a,c)) -> a -> IntervalMap k b -> (a, IntervalMap k c)
mapAccumWithKey f = go
  where
    go a Nil               = (a,Nil)
    go a (Node c kx m x l r) =
                 let (a1,l') = go a l
                     (a2,x') = f a1 kx x
                     (a3,r') = go a2 r
                 in (a3, Node c kx m x' l' r')

-- | /O(n)/. The function 'mapAccumRWithKey' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> k -> b -> (a,c)) -> a -> IntervalMap k b -> (a, IntervalMap k c)
mapAccumRWithKey f = go
  where
    go a Nil = (a, Nil)
    go a (Node c kx m x l r) =
                 let (a1,r') = go a r
                     (a2,x') = f a1 kx x
                     (a3,l') = go a2 l
                 in (a3, Node c kx m x' l' r')


-- | /O(n log n)/. @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
-- 
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the smallest of
-- these keys is retained.
mapKeys :: (Interval k2 e, Ord k2) => (k1 -> k2) -> IntervalMap k1 a -> IntervalMap k2 a
mapKeys f m = fromList [ (f k, v) | (k, v) <- toDescList m ]

-- | /O(n log n)/. @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
-- 
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
mapKeysWith :: (Interval k2 e, Ord k2) => (a -> a -> a) -> (k1 -> k2) -> IntervalMap k1 a -> IntervalMap k2 a
mapKeysWith c f m = fromListWith c [ (f k, v) | (k, v) <- toAscList m ]

-- | /O(n)/. @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
mapKeysMonotonic :: (Interval k2 e, Ord k2) => (k1 -> k2) -> IntervalMap k1 a -> IntervalMap k2 a
mapKeysMonotonic _ Nil = Nil
mapKeysMonotonic f (Node c k _ x l r) =
    mNode c (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)

-- | /O(n)/. Filter values satisfying a predicate.
filter :: (Interval k e) => (a -> Bool) -> IntervalMap k a -> IntervalMap k a
filter p m = filterWithKey (\_ v -> p v) m

-- | /O(n)/. Filter keys\/values satisfying a predicate.
filterWithKey :: (Interval k e) => (k -> a -> Bool) -> IntervalMap k a -> IntervalMap k a
filterWithKey p m = mapMaybeWithKey (\k v -> if p k v then Just v else Nothing) m

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
partition :: (Interval k e) => (a -> Bool) -> IntervalMap k a -> (IntervalMap k a, IntervalMap k a)
partition p m = partitionWithKey (\_ v -> p v) m

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
partitionWithKey :: (Interval k e) => (k -> a -> Bool) -> IntervalMap k a -> (IntervalMap k a, IntervalMap k a)
partitionWithKey p m = mapEitherWithKey p' m
  where
    p' k v | p k v     = Left v
           | otherwise = Right v

-- | /O(n)/. Map values and collect the 'Just' results.
mapMaybe :: (Interval k e) => (a -> Maybe b) -> IntervalMap k a -> IntervalMap k b
mapMaybe f m = mapMaybeWithKey (\_ v -> f v) m

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
mapMaybeWithKey :: (Interval k e) => (k -> a -> Maybe b) -> IntervalMap k a -> IntervalMap k b
mapMaybeWithKey f m = fromDistinctAscList (mapf [] m)
  where
    mapf z Nil = z
    mapf z (Node _ k _ v l r) = mapf (f' k v z r) l
    f' k v z r = case f k v of
                   Nothing -> mapf z r
                   Just v' -> (k,v') : mapf z r

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
mapEither :: (Interval k e) => (a -> Either b c) -> IntervalMap k a -> (IntervalMap k b, IntervalMap k c)
mapEither f m = mapEitherWithKey (\_ v -> f v) m

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: (Interval i k) => (i -> a -> Either b c) -> IntervalMap i a -> (IntervalMap i b, IntervalMap i c)
mapEitherWithKey f m = (fromDistinctAscList l, fromDistinctAscList r)
  where
    (l, r) = part [] [] (toDescList m)
    part ls rs [] = (ls, rs)
    part ls rs ((k,v):xs) = case f k v of
                              Left v'  -> part ((k,v'):ls) rs xs
                              Right v' -> part ls ((k,v'):rs) xs

-- | /O(n)/. The expression (@'split' k map@) is a pair @(map1,map2)@ where
-- the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@.
-- Any key equal to @k@ is found in neither @map1@ nor @map2@.
split :: (Interval i k, Ord i) => i -> IntervalMap i a -> (IntervalMap i a, IntervalMap i a)
split x m = (l, r)
  where (l, _, r) = splitLookup x m
     
-- | /O(n)/. The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@.
splitLookup :: (Interval i k, Ord i) => i -> IntervalMap i a -> (IntervalMap i a, Maybe a, IntervalMap i a)
splitLookup x m = case span (\(k,_) -> k < x) (toAscList m) of
                    ([], [])                        -> (empty, Nothing, empty)
                    ([], ((k,v):_))     | k == x    -> (empty, Just v, deleteMin m)
                                        | otherwise -> (empty, Nothing, m)
                    (_, [])                         -> (m, Nothing, empty)
                    (lt, ge@((k,v):gt)) | k == x    -> (fromDistinctAscList lt, Just v, fromDistinctAscList gt)
                                        | otherwise -> (fromDistinctAscList lt, Nothing, fromDistinctAscList ge)


-- | /O(n)/. Split around a point.
-- Splits the map into three submaps: intervals below the point,
-- intervals containing the point, and intervals above the point.
splitAt :: (Interval i k) => IntervalMap i a -> k -> (IntervalMap i a, IntervalMap i a, IntervalMap i a)
splitAt mp p = (fromUnion (lower mp), mp `containing` p, fromUnion (higher mp))
  where
    lower Nil = UEmpty
    lower s@(Node _ k m v l r)
      | p `above`  m  =  UAppend s UEmpty
      | p `below`  k  =  lower l
      | p `inside` k  =  mkUnion (lower l) (lower r)
      | otherwise     =  mkUnion (lower l) (UCons k v (lower r))
    higher Nil = UEmpty
    higher (Node _ k m v l r)
      | p `above`  m  =  UEmpty
      | p `below`  k  =  mkUnion (higher l) (UCons k v (UAppend r UEmpty))
      | otherwise     =  higher r

-- | /O(n)/. Split around an interval.
-- Splits the set into three subsets: intervals below the given interval,
-- intervals intersecting the given interval, and intervals above the
-- given interval.
splitIntersecting :: (Interval i k, Ord i) => IntervalMap i a -> i -> (IntervalMap i a, IntervalMap i a, IntervalMap i a)
splitIntersecting mp i = (fromUnion (lower mp), mp `intersecting` i, fromUnion (higher mp))
  where
    lower Nil = UEmpty
    lower s@(Node _ k m v l r)
      -- whole set lower: all
      | i `after`  m   =  UAppend s UEmpty
      -- interval before key: only from left subtree
      | i <= k         =  lower l
      -- interval intersects key to the right: both subtrees could contain lower intervals
      | i `overlaps` k =  mkUnion (lower l) (lower r)
      -- interval to the right of the key: key and both subtrees
      | otherwise      =  mkUnion (lower l) (UCons k v (lower r))
    higher Nil = UEmpty
    higher (Node _ k m v l r)
      -- whole set lower: nothing
      | i `after` m    =  UEmpty
      -- interval before key: node and complete right subtree + maybe part of the left subtree
      | i `before`  k  =  mkUnion (higher l) (UCons k v (UAppend r UEmpty))
      -- interval overlaps or to the right of key: only from right subtree
      | otherwise      =  higher r


-- Helper for building sets from distinct ascending keys and submaps
data Union k v = UEmpty | Union !(Union k v) !(Union k v)
               | UCons !k v !(Union k v)
               | UAppend !(IntervalMap k v) !(Union k v)

mkUnion :: Union k v -> Union k v -> Union k v
mkUnion UEmpty u = u
mkUnion u UEmpty = u
mkUnion u1 u2 = Union u1 u2

fromUnion :: Interval k e => Union k v -> IntervalMap k v
fromUnion UEmpty               = empty
fromUnion (UCons key v UEmpty) = singleton key v
fromUnion (UAppend mp UEmpty)  = turnBlack mp
fromUnion x                    = fromDistinctAscList (unfold x [])
  where
    unfold UEmpty        r = r
    unfold (Union a b)   r = unfold a (unfold b r)
    unfold (UCons k v u) r = (k,v) : unfold u r
    unfold (UAppend s u) r = toAscList' s (unfold u r)


-- submaps

-- | /O(n+m)/. This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: (Ord k, Eq a) => IntervalMap k a -> IntervalMap k a -> Bool
isSubmapOf m1 m2 = isSubmapOfBy (==) m1 m2

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and @f@ returns 'True' when
 applied to their respective values.
-}
isSubmapOfBy :: Ord k => (a -> b -> Bool) -> IntervalMap k a -> IntervalMap k b -> Bool
isSubmapOfBy f m1 m2 = ascListSubset f (toAscList m1) (toAscList m2)

ascListSubset :: Ord k => (a -> b -> Bool) -> [(k,a)] -> [(k,b)] -> Bool
ascListSubset _ []    _  =  True
ascListSubset _ (_:_) [] =  False
ascListSubset f s1@((k1,v1):r1) ((k2,v2):r2) =
  case compare k1 k2 of
    GT -> ascListSubset f s1 r2
    EQ -> f v1 v2 && ascListSubset f r1 r2
    LT -> False

-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal). 
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (Ord k, Eq a) => IntervalMap k a -> IntervalMap k a -> Bool
isProperSubmapOf m1 m2 = isProperSubmapOfBy (==) m1 m2

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values.
-}
isProperSubmapOfBy :: Ord k => (a -> b -> Bool) -> IntervalMap k a -> IntervalMap k b -> Bool
isProperSubmapOfBy f m1 m2 = go (toAscList m1) (toAscList m2)
  where
    go [] (_:_)  =  True
    go _  []     =  False
    go s1@((k1,v1):r1) ((k2,v2):r2) =
       case compare k1 k2 of
         GT -> ascListSubset f s1 r2
         EQ -> f v1 v2 && go r1 r2
         LT -> False


-- debugging

-- | Check red-black-tree and interval search augmentation invariants.
-- For testing/debugging only.
valid :: (Interval i k, Ord i) => IntervalMap i v -> Bool
valid mp = test mp && height mp <= maxHeight (size mp) && validColor mp
  where
    test Nil = True
    test n@(Node _ _ _ _ l r) = validOrder n && validMax n && test l && test r
    validMax (Node _ k m _ lo hi) =  m == maxUpper k lo hi
    validMax Nil = True

    validOrder (Node _ _ _ _ Nil Nil) = True
    validOrder (Node _ k1 _ _ Nil (Node _ k2 _ _ _ _)) = k1 < k2
    validOrder (Node _ k2 _ _ (Node _ k1 _ _ _ _) Nil) = k1 < k2
    validOrder (Node _ k2 _ _ (Node _ k1 _ _ _ _) (Node _ k3 _ _ _ _)) = k1 < k2 && k2 < k3
    validOrder Nil = True

    -- validColor parentColor blackCount tree
    validColor n = blackDepth n >= 0

    -- return -1 if subtrees have diffrent black depths or two consecutive red nodes are encountered
    blackDepth :: IntervalMap k v -> Int
    blackDepth Nil  = 0
    blackDepth (Node c _ _ _ l r) = case blackDepth l of
                                      ld -> if ld < 0 then ld
                                            else
                                              case blackDepth r of
                                                rd -> if rd < 0 then rd
                                                      else if rd /= ld then -1
                                                      else if c == R && (isRed l || isRed r) then -1
                                                      else if c == B then rd + 1
                                                      else rd

