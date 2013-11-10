module ArrayMutation where

import Control.Applicative
import Control.Monad
import Data.Array.MArray hiding (inRange, mapArray)

-- | Compose a 2-ary function with a 1-ary function
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.)(.)(.)

-- | Is a between b and c?
inRange :: Ord a => a -> (a, a) -> Bool
inRange a (b,c) = min b c <= a && a <= max b c

-- | Make a be between b and c!
clip :: Ord a => a -> (a,a) -> a
clip a (b,c) | (min b c) > a = min b c
clip a (b,c) | a > (max b c) = max b c
clip a (b,c) = a

-- | If the intervals given overlap, then return just their intersection. Otherwise, return nothing. This operation is symmetric, except that it privileges the *sign* of the first interval.
clipInterval :: Ord t => (t, t) -> (t, t) -> Maybe (t, t)
clipInterval (a,b) (c,d) =
   if a `inRange` (c,d) || b `inRange` (c,d)
   || c `inRange` (a,b) || d `inRange` (a,b)
   then Just (clip a (c,d), clip b (c,d))
   else Nothing

-- | Enumerate an interval, backwards if the first index is after the last.
enumInterval :: (Enum a, Ord a) => (a, a) -> [a]
enumInterval (l,h) | l <= h    = enumFromTo l h
enumInterval (l,h) | otherwise = enumFromThenTo l (pred l) h

-- | Destructive map with accumulator over an interval of the array. Like mapAccum, cannot change the shape of the array, only its contents, but gets additional context from the accumulator parameter. With an interval (a,b) s.t. a > b, moves right to left over the array.
{-# INLINE mapAccumArrayInterval #-}
mapAccumArrayInterval :: (Enum i, Functor m, Ix i, MArray a e m) => a i e -> (acc -> e -> (acc, e)) -> acc -> (i, i) -> m acc
mapAccumArrayInterval a f acc interval =
   maybe [] enumInterval . clipInterval interval <$> getBounds a >>=
   flip foldM acc
      (\acc i -> f acc <$> readArray a i >>=
                 (>>) <$> writeArray a i . snd
                      <*>         return . fst)

-- | Throws away the accumulator result from mapAccumArrayInterval
{-# INLINE mapAccumArrayInterval_ #-}
mapAccumArrayInterval_ :: (Enum i, Functor m, Ix i, MArray a e m) => a i e -> (acc -> e -> (acc, e)) -> acc -> (i, i) -> m ()
mapAccumArrayInterval_ a f acc interval =
   mapAccumArrayInterval a f acc interval >> return ()

-- | Like mapAccumArrayInterval, but over the whole array, left to right.
{-# INLINE mapAccumArrayL #-}
mapAccumArrayL :: (Enum i, Functor m, Ix i, MArray a e m) => a i e -> (b -> e -> (b, e)) -> b -> m b
mapAccumArrayL a f acc =
   getBounds a >>= mapAccumArrayInterval a f acc

-- | Like mapAccumArrayL, but throws away the accumulator return value.
{-# INLINE mapAccumArrayL_ #-}
mapAccumArrayL_ :: (Enum i, Functor m, Ix i, MArray a e m) => a i e -> (acc -> e -> (acc, e)) -> acc -> m ()
mapAccumArrayL_ a f acc = mapAccumArrayL a f acc >> return ()

-- | Like mapAccumArrayInterval, but over the whole array, right to left.
{-# INLINE mapAccumArrayR #-}
mapAccumArrayR :: (Enum i, Functor m, Ix i, MArray a e m) => a i e -> (b -> e -> (b, e)) -> b -> m b
mapAccumArrayR a f acc =
   uncurry (flip (,)) <$> getBounds a >>= mapAccumArrayInterval a f acc

-- | Like mapAccumArrayR, but throws away the accumulator return value.
{-# INLINE mapAccumArrayR_ #-}
mapAccumArrayR_ :: (Enum i, Functor m, Ix i, MArray a e m) => a i e -> (acc -> e -> (acc, e)) -> acc -> m ()
mapAccumArrayR_ a f acc = mapAccumArrayR a f acc >> return ()

-- | Maps a function over an interval of an array (no accumulator).
{-# INLINE mapArrayInterval_ #-}
mapArrayInterval_ :: (Enum i, Functor m, Ix i, MArray a e m) => a i e -> (e -> e) -> (i, i) -> m ()
mapArrayInterval_ a f interval =
   mapAccumArrayInterval a (\acc e -> ((),f e)) () interval >> return ()

-- | Maps a function over an entire array (no accumulator).
{-# INLINE mapArray_ #-}
mapArray_ :: (Enum i, Functor m, Ix i, MArray a e m) => a i e -> (e -> e) -> m ()
mapArray_ a f =
   getBounds a >>= mapArrayInterval_ a f >> return ()
