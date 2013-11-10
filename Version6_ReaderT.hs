module Main ( main ) where

import Common

import Data.Ord
import Data.Function
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.ST
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Data.Array.IO
import Data.Array.MArray

----------------------------------------

maybeReadBy :: (Functor m, Ix i, MArray a e m) => a i e -> (e -> Bool) -> i -> m (Maybe e)
maybeReadBy arr predicate n = do
   (minIndex, maxIndex) <- getBounds arr
   if n < minIndex || maxIndex < n then return Nothing
   else (\x -> if not (predicate x) then Nothing else Just x) <$> readArray arr n

safeWriteArray :: (Ix i, MArray a e m) => a i e -> i -> e -> m ()
safeWriteArray arr i e = do
   (minIndex, maxIndex) <- getBounds arr
   if i < minIndex || maxIndex < i
      then return ()
      else writeArray arr i e

writeTVarIf :: TVar a -> (a -> Bool) -> a -> IO ()
writeTVarIf var predicate new = atomically $ do
   current <- readTVar var
   when (predicate current) $ writeTVar var new

writeIORefIf :: IORef a -> (a -> Bool) -> a -> IO ()
writeIORefIf ref predicate new = do
   current <- readIORef ref
   when (predicate current) $ writeIORef ref new

enumInterval :: (Enum a, Ord a) => (a, a) -> [a]
enumInterval (l,h) | l <= h    = enumFromTo l h
enumInterval (l,h) | otherwise = enumFromThenTo l (pred l) h

----------------------------------------

collatzLength :: IORef (Int,Int) -> Int -> ReaderT Config IO Int
collatzLength _        1 = return 0
collatzLength localMax n = do
   lengths <- asks lengthsArray
   (liftIO $ maybeReadBy lengths (>= 0) n) >>=
      (flip maybe return $ do
         len <- (1+) <$> collatzLength localMax (collatz n)
         liftIO $ safeWriteArray lengths n len
         liftIO $ writeIORefIf localMax ((< len) . snd) (n,len)
         return len)

maxChainIn :: (Int,Int) -> ReaderT Config IO (Int,Int)
maxChainIn interval = do 
   localMax <- liftIO $ newIORef (1,0)
   mapM_ (collatzLength localMax) $ enumInterval interval
   liftIO $ readIORef localMax

getWorkChunk :: ReaderT Config IO (Maybe (Int,Int))
getWorkChunk = do 
   theMaxNumber     <- asks maxNumber
   theChunkSize     <- asks chunkSize
   theCurrentNumber <- asks currentNumber
   liftIO $ atomically $ do
      oldNumber    <- readTVar theCurrentNumber
      let newNumber = min theMaxNumber (oldNumber + theChunkSize)
      writeTVar theCurrentNumber (newNumber + 1)
      return $ if oldNumber >= newNumber
                  then Nothing
                  else Just (oldNumber,newNumber)

workerThread :: MVar () -> ReaderT Config IO ()
workerThread flag = do
   theGlobalMax <- asks globalMax
   chunk <- getWorkChunk
   case chunk of
      Just interval -> do
         (n,len) <- maxChainIn interval
         liftIO $ writeTVarIf theGlobalMax ((< len) . snd) (n,len)
         workerThread flag
      Nothing -> liftIO $ putMVar flag ()

spawnWorker :: ReaderT Config IO (MVar ())
spawnWorker = do
   config <- ask
   flag <- liftIO $ newEmptyMVar
   _ <- liftIO $ forkIO $ runReaderT (workerThread flag) config
   return flag

newEmptyLengths :: Int -> IO (IOUArray Int Int)
newEmptyLengths maxNumber =
   newArray (1,maxNumber) (-1) >>= \a -> writeArray a 1 0 >> return a

data Config = Config { cores         :: Int
                     , chunkSize     :: Int
                     , maxNumber     :: Int
                     , lengthsArray  :: IOUArray Int Int
                     , globalMax     :: TVar (Int,Int)
                     , currentNumber :: TVar Int }

main :: IO ()
main = do
   maxNum <- getMaxNumber
   config <- Config <$> getNumCapabilities
                    <*> pure (10^4)
                    <*> pure maxNum
                    <*> newEmptyLengths maxNum
                    <*> newTVarIO (1,0)
                    <*> newTVarIO 1
   mapM_ takeMVar =<< (sequence $ replicate (cores config)
                                $ runReaderT spawnWorker config)
   print =<< readTVarIO (globalMax config)

