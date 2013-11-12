module MutableArray
   ( newEmptyLengths
   , safeReadArray
   , safeWriteArray
   , module Data.Array.ST
   , module Data.Array.IO ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array.ST
import Data.Array.IO

-- | Make a new array of indices 1 .. maxNumber, with all values set to -1, except that at 1, which is set to 0
newEmptyLengths :: (Num i, Num e, Ix i, MArray a e m) => i -> m (a i e)
newEmptyLengths maxNumber =
   newArray (1,maxNumber) (-1) >>= \a -> writeArray a 1 0 >> return a

-- | Tests if a value is between the fst element of the tuple and the snd, inclusive
insideRange :: Ord a => a -> (a, a) -> Bool
insideRange i = uncurry (&&) . first (<= i) . second (i <=)

-- | Returns Just an element of the array if the index is in bounds; otherwise Nothing
{-# INLINE safeReadArray #-}
safeReadArray :: (Functor m, Ix i, MArray a e m) => a i e -> i -> m (Maybe e)
safeReadArray arr i = do
   safe <- insideRange i <$> getBounds arr
   if safe then Just <$> readArray arr i
           else return Nothing

-- | Writes to the array if the index is in bounds; otherwise, is a no-op
{-# INLINE safeWriteArray #-}
safeWriteArray :: (Functor m, Ix i, MArray a e m) => a i e -> i -> e -> m ()
safeWriteArray arr i e = do
   safe <- insideRange i <$> getBounds arr
   when safe $ writeArray arr i e
