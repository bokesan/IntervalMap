-- Version of IntervalMap where Color is embedded in Node constructor.
-- Only lookup and fromDistinctAscList are supported
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module RBColorNodeBase (
            -- * re-export
            Interval(..)
            -- * Map type
            , IntervalMap(..)      -- instance Eq,Show,Read

            -- * Operators
            , (!)

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

            -- * Conversion
            , elems
            , keys
            , keysSet
            , assocs

            -- ** Lists
            , toList

            -- ** Ordered lists
            , toAscList
            , toDescList
            , fromAscList
            , fromAscListWith
            , fromAscListWithKey
            , fromDistinctAscList

            -- * Internal, not re-exported by Data.IntervalMap.{Lazy,Strict}
            , turnBlack

            -- * Testing
            , height, maxHeight, showStats

            ) where

import Prelude hiding (null, lookup, map, filter, foldr, foldl)
import Data.Bits (shiftR, (.&.))
import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(..), (<$>))
import Data.Traversable (Traversable(traverse))
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import qualified Data.Set as Set
import Control.DeepSeq

import Data.IntervalMap.Generic.Interval

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !

-- | /O(log n)/. Lookup value for given key. Calls 'error' if the key is not in the map.
--
-- Use 'lookup' or 'findWithDefault' instead of this function, unless you are absolutely
-- sure that the key is present in the map.
(!) :: (Interval k e, Ord k) => IntervalMap k v -> k -> v
tree ! key = case lookup key tree of
               Just v  -> v
               Nothing -> error "IntervalMap.!: key not found"


-- | A map from intervals of type @k@ to values of type @v@.
data IntervalMap k v = Nil
                      | NodeR
                             !k -- key
                             !k -- interval with maximum upper in tree
                             v             -- value
                             !(IntervalMap k v) -- left subtree
                             !(IntervalMap k v) -- right subtree
                      | NodeB
                             !k -- key
                             !k -- interval with maximum upper in tree
                             v             -- value
                             !(IntervalMap k v) -- left subtree
                             !(IntervalMap k v) -- right subtree

instance (Eq k, Eq v) => Eq (IntervalMap k v) where
  a == b = toAscList a == toAscList b

instance (Ord k, Ord v) => Ord (IntervalMap k v) where
  compare a b = compare (toAscList a) (toAscList b)

instance Functor (IntervalMap k) where
  fmap f m  = map f m

instance Traversable (IntervalMap k) where
  traverse _ Nil = pure Nil
  traverse f (NodeR k m v l r) = flip (NodeR k m) <$> traverse f l <*> f v <*> traverse f r
  traverse f (NodeB k m v l r) = flip (NodeB k m) <$> traverse f l <*> f v <*> traverse f r

instance Foldable.Foldable (IntervalMap k) where
  fold Nil = mempty
  fold (NodeR _ _ v l r) = Foldable.fold l `mappend` v `mappend` Foldable.fold r
  fold (NodeB _ _ v l r) = Foldable.fold l `mappend` v `mappend` Foldable.fold r
  foldr = foldr
  foldl = foldl
  foldMap _ Nil = mempty
  foldMap f (NodeR _ _ v l r) = Foldable.foldMap f l `mappend` f v `mappend` Foldable.foldMap f r
  foldMap f (NodeB _ _ v l r) = Foldable.foldMap f l `mappend` f v `mappend` Foldable.foldMap f r

instance (NFData k, NFData a) => NFData (IntervalMap k a) where
    rnf Nil = ()
    rnf (NodeR kx _ x l r) = kx `deepseq` x `deepseq` l `deepseq` r `deepseq` ()
    rnf (NodeB kx _ x l r) = kx `deepseq` x `deepseq` l `deepseq` r `deepseq` ()

instance (Show k, Show a) => Show (IntervalMap k a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)


isRed :: IntervalMap k v -> Bool
isRed (NodeR _ _ _ _ _) = True
isRed _ = False

turnBlack :: IntervalMap k v -> IntervalMap k v
turnBlack (NodeR k m vs l r) = NodeB k m vs l r
turnBlack t = t

turnRed :: IntervalMap k v -> IntervalMap k v
turnRed Nil = error "turnRed: Leaf"
turnRed (NodeB k m v l r) = NodeR k m v l r
turnRed t = t

data Color = Red | Black
            
-- construct node, recomputing the upper key bound.
mNode :: (Interval k e) => Color -> k -> v -> IntervalMap k v -> IntervalMap k v -> IntervalMap k v
mNode Red   k v l r = NodeR k (maxUpper k l r) v l r
mNode Black k v l r = NodeB k (maxUpper k l r) v l r

maxUpper :: (Interval i k) => i -> IntervalMap i v -> IntervalMap i v -> i
maxUpper k Nil                Nil                 = k `seq` k
maxUpper k Nil                (NodeR _ m _ _ _) = maxByUpper k m
maxUpper k Nil                (NodeB _ m _ _ _) = maxByUpper k m
maxUpper k (NodeR _ m _ _ _) Nil                 = maxByUpper k m
maxUpper k (NodeR _ l _ _ _) (NodeR _ r _ _ _) = maxByUpper k (maxByUpper l r)
maxUpper k (NodeR _ l _ _ _) (NodeB _ r _ _ _) = maxByUpper k (maxByUpper l r)
maxUpper k (NodeB _ m _ _ _) Nil                 = maxByUpper k m
maxUpper k (NodeB _ l _ _ _) (NodeR _ r _ _ _) = maxByUpper k (maxByUpper l r)
maxUpper k (NodeB _ l _ _ _) (NodeB _ r _ _ _) = maxByUpper k (maxByUpper l r)

-- interval with the greatest upper bound. The lower bound is ignored!
maxByUpper :: (Interval i e) => i -> i -> i
maxByUpper a b | rightClosed a = if upperBound a >= upperBound b then a else b
               | otherwise     = if upperBound a >  upperBound b then a else b

-- ---------------------------------------------------------

-- | /O(1)/. The empty map.
empty :: IntervalMap k v
empty =  Nil

-- | /O(1)/. A map with one entry.
singleton :: k -> v -> IntervalMap k v
singleton k v = NodeB k k v Nil Nil


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
                      NodeR _ _ _ l r -> h (h n l + 1) r
                      NodeB _ _ _ l r -> h (h n l + 1) r

-- | The height of the tree. For testing/debugging only.
height :: IntervalMap k v -> Int
height Nil = 0
height (NodeR _ _ _ l r) = 1 + max (height l) (height r)
height (NodeB _ _ _ l r) = 1 + max (height l) (height r)

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
lookup k (NodeR key _ v l r) = case compare k key of
                                  LT -> lookup k l
                                  GT -> lookup k r
                                  EQ -> Just v
lookup k (NodeB key _ v l r) = case compare k key of
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

-- | Return all key/value pairs where the key intervals contain the given point.
-- The elements are returned in ascending key order.
--
-- /O(n)/, since potentially all keys could contain the point.
-- /O(log n)/ average case. This is also the worst case for maps containing no overlapping keys.
containing :: (Interval k e) => IntervalMap k v -> e -> IntervalMap k v
t `containing` pt = fromDistinctAscList (go [] pt t)
  where
    go xs p Nil = p `seq` xs
    go xs p (NodeR k m v l r)
       | p `above` m  =  xs         -- above all intervals in the tree: no result
       | p `below` k  =  go xs p l  -- to the left of the lower bound: can't be in right subtree
       | p `inside` k =  go ((k,v) : go xs p r) p l
       | otherwise    =  go (go xs p r) p l
    go xs p (NodeB k m v l r)
       | p `above` m  =  xs         -- above all intervals in the tree: no result
       | p `below` k  =  go xs p l  -- to the left of the lower bound: can't be in right subtree
       | p `inside` k =  go ((k,v) : go xs p r) p l
       | otherwise    =  go (go xs p r) p l

-- | Return all key/value pairs where the key intervals overlap (intersect) the given interval.
-- The elements are returned in ascending key order.
--
-- /O(n)/, since potentially all keys could intersect the interval.
-- /O(log n)/ average case, if few keys intersect the interval.
intersecting :: (Interval k e) => IntervalMap k v -> k -> IntervalMap k v
t `intersecting` iv = fromDistinctAscList (go [] iv t)
  where
    go xs i Nil = i `seq` xs
    go xs i (NodeR k m v l r)
       | i `after` m     =  xs
       | i `before` k    =  go xs i l
       | i `overlaps` k  =  go ((k,v) : go xs i r) i l
       | otherwise       =  go (go xs i r) i l
    go xs i (NodeB k m v l r)
       | i `after` m     =  xs
       | i `before` k    =  go xs i l
       | i `overlaps` k  =  go ((k,v) : go xs i r) i l
       | otherwise       =  go (go xs i r) i l

-- | Return all key/value pairs where the key intervals are completely inside the given interval.
-- The elements are returned in ascending key order.
--
-- /O(n)/, since potentially all keys could be inside the interval.
-- /O(log n)/ average case, if few keys are inside the interval.
within :: (Interval k e) => IntervalMap k v -> k -> IntervalMap k v
t `within` iv = fromDistinctAscList (go [] iv t)
  where
    go xs i Nil = i `seq` xs
    go xs i (NodeR k m v l r)
       | i `after` m     =  xs
       | i `before` k    =  go xs i l
       | i `subsumes` k  =  go ((k,v) : go xs i r) i l
       | otherwise       =  go (go xs i r) i l
    go xs i (NodeB k m v l r)
       | i `after` m     =  xs
       | i `before` k    =  go xs i l
       | i `subsumes` k  =  go ((k,v) : go xs i r) i l
       | otherwise       =  go (go xs i r) i l

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
foldr :: (a -> b -> b) -> b -> IntervalMap k a -> b
foldr _ z Nil = z
foldr f z (NodeR _ _ x l r) = foldr f (f x (foldr f z r)) l
foldr f z (NodeB _ _ x l r) = foldr f (f x (foldr f z r)) l

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
foldl :: (b -> a -> b) -> b -> IntervalMap k a -> b
foldl _ z Nil = z
foldl f z (NodeR _ _ x l r) = foldl f (f (foldl f z l) x) r
foldl f z (NodeB _ _ x l r) = foldl f (f (foldl f z l) x) r

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
foldrWithKey :: (k -> v -> a -> a) -> a -> IntervalMap k v -> a
foldrWithKey _ z Nil = z
foldrWithKey f z (NodeR k _ x l r) = foldrWithKey f (f k x (foldrWithKey f z r)) l
foldrWithKey f z (NodeB k _ x l r) = foldrWithKey f (f k x (foldrWithKey f z r)) l

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
foldlWithKey :: (a -> k -> v -> a) -> a -> IntervalMap k v -> a
foldlWithKey _ z Nil = z
foldlWithKey f z (NodeR k _ x l r) = foldlWithKey f (f (foldlWithKey f z l) k x) r
foldlWithKey f z (NodeB k _ x l r) = foldlWithKey f (f (foldlWithKey f z l) k x) r

-- | /O(n)/. Map a function over all values in the map.
map :: (a -> b) -> IntervalMap k a -> IntervalMap k b
map f = mapWithKey (\_ x -> f x)

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (k -> a -> b) -> IntervalMap k a -> IntervalMap k b
mapWithKey f = go
  where
    go Nil = Nil
    go (NodeR k m v l r) = NodeR k m (f k v) (go l) (go r)
    go (NodeB k m v l r) = NodeB k m (f k v) (go l) (go r)

-- --- Conversion ---

-- | /O(n)/. The list of all key\/value pairs contained in the map, in ascending order of keys.
toAscList :: IntervalMap k v -> [(k,v)]
toAscList m = foldrWithKey (\k v r -> (k,v) : r) [] m

-- | /O(n)/. The list of all key\/value pairs contained in the map, in no particular order.
toList :: IntervalMap k v -> [(k,v)]
toList m = toAscList m

-- | /O(n)/. The list of all key\/value pairs contained in the map, in descending order of keys.
toDescList :: IntervalMap k v -> [(k, v)]
toDescList m = foldlWithKey (\r k v -> (k,v) : r) [] m

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
                | n == 1     = case xs of ((k,v):xs') -> T2 (NodeB k k v Nil Nil) xs'
                                          _ -> error "fromDictinctAscList: buildB 1"
                | otherwise  =
                     case n `quot` 2 of { n' ->
                     case buildB n' xs of { (T2 _ []) -> error "fromDictinctAscList: buildB n";
                                            (T2 l ((k,v):xs')) ->
                     case buildB n' xs' of { (T2 r xs'') ->
                     T2 (mNode Black k v l r) xs'' }}}

    buildR n d xs | d `seq` xs `seq` n == 0 = T2 Nil xs
                  | n == 1    = case xs of ((k,v):xs') -> T2 (if d == 0 then NodeR k k v Nil Nil
                                                                        else NodeB k k v Nil Nil)
                                                             xs'
                                           _ -> error "fromDistinctAscList: buildR 1"
                  | otherwise =
                      case n `quot` 2 of { n' ->
                      case buildR n' (d-1) xs of { (T2 _ []) -> error "fromDistinctAscList: buildR n";
                                                   (T2 l ((k,v):xs')) ->
                      case buildR (n - (n' + 1)) (d-1) xs' of { (T2 r xs'') ->
                      T2 (mNode Black k v l r) xs'' }}}


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
keysSet :: (Ord k) => IntervalMap k v -> Set.Set k
keysSet m =  Set.fromDistinctAscList (keys m)

-- | Same as 'toAscList'.
assocs :: IntervalMap k v -> [(k, v)]
assocs m = toAscList m

