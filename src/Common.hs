
module Common 
   ( collatz
   , getFlooredArg , getMaxNumber , getChunkSize , getCacheSize , getNumCores
   , defaultMaxNumber , defaultChunkSize
   , module Data.Ord
   , module Data.List
   , module Control.Arrow
   , module Control.Applicative
   , module Control.Monad
   , module Control.Monad.State
   , module Data.IntMap
   , module Data.STRef
   , module Data.Array.ST
   , module Control.Monad.ST
   , module Data.IORef
   , module Control.Concurrent
   , module Control.Concurrent.STM ) where

import System.Environment
import Data.Maybe

-- All these are meant to be re-exported...
import Data.Ord
import Data.List
import Control.Arrow
import Control.Applicative
import Control.Monad
-- Version 3 needs...
import Control.Monad.State
import Data.IntMap (IntMap)
-- Versions 4 & 5 need...
import Data.STRef
import Data.Array.ST
import Control.Monad.ST
-- Version 6 needs...
import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM

--------------------------------------------------

-- For all implementations:

-- | The recurrence relation defining the Collatz sequences
collatz :: Int -> Int
collatz n =
   if even n
      then n `div` 2
      else 3 * n + 1

-- | The default maximum number; this is the problem size on Project Euler
defaultMaxNumber = 10^6

-- | Get the nth (zero-indexed) command line argument as an integer, but parse the argument as a float
-- | to allow floating-point syntax for specifying large numbers
getFlooredArg :: Int -> Int -> IO Int
getFlooredArg n defaultValue =
   maybe defaultValue (floor . read) .
                      (\x -> case x of
                           (Just "_") -> Nothing
                           (Just x)   -> (Just x)
                           Nothing    -> Nothing) .
                      listToMaybe .
                      drop n <$> getArgs

-- | Get the maximum number specified by the user, with a default of... the default
getMaxNumber = getFlooredArg 0 defaultMaxNumber

-- For parallel implementation:

-- | The default chunk size
defaultChunkSize = 10^4
-- | Get the user-specified chunk size (arg 1) with a default of the default
getChunkSize = getFlooredArg 1 defaultChunkSize
-- | Get the user-specified cache size (arg 2) -- default must be filled in as the problem size
getCacheSize = getFlooredArg 2
-- | Get the user-specified number of cores to use (arg 3) -- default is the number of capabilities
-- | given to the runtime
getNumCores  = getFlooredArg 3 =<< getNumCapabilities
