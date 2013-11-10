
module Common 
   ( collatz
   , getFlooredArg , getMaxNumber , getChunkSize , getCacheSize , getNumCores
   , defaultMaxNumber , defaultChunkSize
   , safeReadArray , safeWriteArray , newEmptyLengths
   , module Data.Ord
   , module Control.Arrow
   , module Data.List
   , module Control.Applicative
   , module Control.Monad
   , module Control.Concurrent
   , module Control.Concurrent.STM
   , module Data.IORef
   , module Data.Array.IO
   , module Data.IntMap
   , module Control.Monad.State
   , module Data.STRef
   , module Control.Monad.ST
   , module Data.Array.ST ) where

import System.Environment
import Data.Maybe

-- All these are meant to be re-exported...
import Data.Ord
import Control.Arrow
import Data.List
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Data.Array.IO
import Data.IntMap (IntMap)
import Control.Monad.State
import Data.STRef
import Control.Monad.ST
import Data.Array.ST

--------------------------------------------------

-- For all implementations:

collatz :: Int -> Int
collatz n =
   if even n
      then n `div` 2
      else 3 * n + 1

defaultMaxNumber = 10^6

getFlooredArg :: Int -> Int -> IO Int
getFlooredArg n defaultValue =
   maybe defaultValue (floor . read) .
                      (\x -> case x of
                           (Just "_") -> Nothing
                           (Just x)   -> (Just x)
                           Nothing    -> Nothing) .
                      listToMaybe .
                      drop n <$> getArgs

getMaxNumber = getFlooredArg 0 defaultMaxNumber

-- For array-based implementations:

newEmptyLengths :: (Num i, Num e, Ix i, MArray a e m) => i -> m (a i e)
newEmptyLengths maxNumber =
   newArray (1,maxNumber) (-1) >>= \a -> writeArray a 1 0 >> return a

insideRange :: Ord a => a -> (a, a) -> Bool
insideRange i = uncurry (&&) . first (<= i) . second (i <=)

{-# INLINE safeReadArray #-}
safeReadArray :: (Functor m, Ix i, MArray a e m) => a i e -> i -> m (Maybe e)
safeReadArray arr i = do
   safe <- insideRange i <$> getBounds arr
   if safe then Just <$> readArray arr i
           else return Nothing

{-# INLINE safeWriteArray #-}
safeWriteArray :: (Functor m, Ix i, MArray a e m) => a i e -> i -> e -> m ()
safeWriteArray arr i e = do
   safe <- insideRange i <$> getBounds arr
   when safe $ writeArray arr i e

-- For parallel implementation:

defaultChunkSize = 10^4

getChunkSize = getFlooredArg 1 defaultChunkSize
getCacheSize = getFlooredArg 2
getNumCores  = getFlooredArg 3
