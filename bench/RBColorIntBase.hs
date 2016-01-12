-- Version of IntervalMap where Color is an Int
-- Only lookup and fromDistinctAscList are supported
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module RBColorIntBase (
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

            -- * Min\/Max
            , findMin
            , findMax
            , findLast

            -- * Internal, not re-exported by Data.IntervalMap.{Lazy,Strict}
            , Color(..)
            , turnBlack

            -- * Debugging
            , valid

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


-- data Color = R | B deriving (Eq, Read, Show)
type Color = Int
cRED, cBLACK :: Int
cRED = 0
cBLACK = 1

-- | A map from intervals of type @k@ to values of type @v@.
data IntervalMap k v = Nil
                      | Node {-# UNPACK #-} !Int
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

instance (Show k, Show a) => Show (IntervalMap k a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)


isRed :: IntervalMap k v -> Bool
isRed (Node 0 _ _ _ _ _) = True
isRed _ = False

turnBlack :: IntervalMap k v -> IntervalMap k v
turnBlack (Node 0 k m vs l r) = Node cBLACK k m vs l r
turnBlack t = t

turnRed :: IntervalMap k v -> IntervalMap k v
turnRed Nil = error "turnRed: Leaf"
turnRed (Node 1 k m v l r) = Node cRED k m v l r
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
maxByUpper a b | rightClosed a = if upperBound a >= upperBound b then a else b
               | otherwise     = if upperBound a >  upperBound b then a else b

-- ---------------------------------------------------------

-- | /O(1)/. The empty map.
empty :: IntervalMap k v
empty =  Nil

-- | /O(1)/. A map with one entry.
singleton :: k -> v -> IntervalMap k v
singleton k v = Node cBLACK k k v Nil Nil


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

-- | Return all key/value pairs where the key intervals contain the given point.
-- The elements are returned in ascending key order.
--
-- /O(n)/, since potentially all keys could contain the point.
-- /O(log n)/ average case. This is also the worst case for maps containing no overlapping keys.
containing :: (Interval k e) => IntervalMap k v -> e -> IntervalMap k v
t `containing` pt = fromDistinctAscList (go [] pt t)
  where
    go xs p Nil = p `seq` xs
    go xs p (Node _ k m v l r)
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
    go xs i (Node _ k m v l r)
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
    go xs i (Node _ k m v l r)
       | i `after` m     =  xs
       | i `before` k    =  go xs i l
       | i `subsumes` k  =  go ((k,v) : go xs i r) i l
       | otherwise       =  go (go xs i r) i l


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

-- | Returns the interval with the largest endpoint.
-- If there is more than one interval with that endpoint,
-- return the rightmost.
--
-- /O(n)/, since all keys could have the same endpoint.
-- /O(log n)/ average case.
findLast :: (Interval k e) => IntervalMap k v -> (k, v)
findLast Nil = error "IntervalMap.findLast: empty map"
findLast t@(Node _ _ mx _ _ _) = lastMax
  where
    (lastMax : _) = go t
    go Nil = []
    go (Node _ k m v l r) | sameU m mx = if sameU k m then go r ++ ((k,v) : go l)
                                                      else go r ++ go l
                          | otherwise  = []
    sameU a b = upperBound a == upperBound b && rightClosed a == rightClosed b


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
                | n == 1     = case xs of ((k,v):xs') -> T2 (Node cBLACK k k v Nil Nil) xs'
                                          _ -> error "fromDictinctAscList: buildB 1"
                | otherwise  =
                     case n `quot` 2 of { n' ->
                     case buildB n' xs of { (T2 _ []) -> error "fromDictinctAscList: buildB n";
                                            (T2 l ((k,v):xs')) ->
                     case buildB n' xs' of { (T2 r xs'') ->
                     T2 (mNode cBLACK k v l r) xs'' }}}

    buildR n d xs | d `seq` xs `seq` n == 0 = T2 Nil xs
                  | n == 1    = case xs of ((k,v):xs') -> T2 (Node (if d==0 then cRED else cBLACK) k k v Nil Nil) xs'
                                           _ -> error "fromDistinctAscList: buildR 1"
                  | otherwise =
                      case n `quot` 2 of { n' ->
                      case buildR n' (d-1) xs of { (T2 _ []) -> error "fromDistinctAscList: buildR n";
                                                   (T2 l ((k,v):xs')) ->
                      case buildR (n - (n' + 1)) (d-1) xs' of { (T2 r xs'') ->
                      T2 (mNode cBLACK k v l r) xs'' }}}


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
                                                      else if c == cRED && (isRed l || isRed r) then -1
                                                      else if c == cBLACK then rd + 1
                                                      else rd

