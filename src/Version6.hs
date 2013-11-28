module Main ( main ) where
import Common
import MutableArray

main :: IO ()
main = print =<< do                      -- This program is multithreaded, so our main does a little more than before:
   cores  <- getNumCores                 -- Find out how many cores the user wants us to use (see Common.hs).
   config <- Config <$> (newEmptyLengths =<< getCacheSize =<< getMaxNumber) -- Create a new Config...
                    <*> getChunkSize <*> getMaxNumber                     -- (for a description of what's stored here,
                    <*> newTVarIO 1  <*> newTVarIO (1,0)                  --  see line 31 of this file.)
   mapM_ waitOnFlag =<< (sequence . replicate cores $ spawnWorker config) -- Spawn workers & wait for them to finish,
   readTVarIO (globalMax config)                                          -- then read off the global maximum.

maxChainIn :: Config -> (Int,Int) -> IO (Int,Int)  -- Find the max chain in an interval; we need to use the IO monad
maxChainIn config (low,high) = do                  -- because the ST monad doesn't permit threads.
   localMax  <- newIORef (1,0)                     -- Make a new thread-local ref to store the local maximum,
   mapM_ (collatzLength config localMax) [low .. high] -- then find the length of all chains in the interval,
   readIORef localMax                                  -- and read off the local maximum.

collatzLength :: Config -> IORef (Int,Int) -> Int -> IO Int
collatzLength _      _        1 = return 0
collatzLength config localMax n =                          -- Everything is the same as Version5, except:
   mfilter (>= 0) <$> safeReadArray (lengths config) n >>= -- We need to explicitly unwrap the lengths array
      (flip maybe return $ do                              -- when we use it, as above,
         len <- (1+) <$> collatzLength config localMax (collatz n)
         safeWriteArray (lengths config) n len             -- and also as we do here.
         currentMax <- snd <$> readIORef localMax
         when (currentMax < len) $ writeIORef localMax (n,len)
         return len)

data Config = Config { lengths       :: IOUArray Int Int   -- a Config lets us store the memoizing array,
                     , chunkSize     :: Int                -- the size of work chunk for threads,
                     , maxNumber     :: Int                -- the maximum number to search till,
                     , currentNumber :: TVar Int           -- the number to be given to the next requesting thread,
                     , globalMax     :: TVar (Int,Int) }   -- and the global maximum value found

spawnWorker :: Config -> IO Flag                         -- how to spawn a new worker thread:
spawnWorker config =                                     -- fork a new thread, giving it a new flag (see lines 49-52)
   (>>) <$> forkIO . worker <*> return =<< newFlag       -- and return that same flag as a result
   where worker flag = getWorkChunk config >>= \c -> case c of  -- try to get a chunk of work to do
            Nothing -> raiseFlag flag                           -- and if there's no work to be done, raise your flag
            Just interval -> do                                 -- otherwise, you got an interval, so:
               (n,len) <- maxChainIn config interval            -- find the max chain in that interval,
               atomically $ do                                  -- and atomically (using Control.Concurrent.STM),
                  currentMax <- snd <$> readTVar (globalMax config)              -- get the current max
                  when (currentMax < len) $ writeTVar (globalMax config) (n,len) -- and update it if necessary
               worker flag                                                       -- then start the cycle over

type Flag  = MVar ()          -- A flag is just an MVar with only an empty and full state.
newFlag    = newEmptyMVar     -- Making a new flag generates it as empty,
raiseFlag  = flip putMVar ()  -- and raising the flag entails filling it.
waitOnFlag = takeMVar         -- MVars block on reads if they're empty, so waiting for a flag entails trying to read.

getWorkChunk :: Config -> IO (Maybe (Int,Int))   -- This provides a way to atomically assign work to threads
getWorkChunk config = atomically $ do            -- with a guarantee of no duplication of work, thanks to STM.
   oldNumber <- readTVar (currentNumber config)  -- Read off the current number stored in the TVar of the config,
   let newNumber = min <$> maxNumber <*> (oldNumber +) . chunkSize $ config  -- and add chunkSize to it.
   writeTVar (currentNumber config) (newNumber + 1)                          -- Then update the current number,
   return . mfilter (fst <**> pure (<=) <*> snd) . Just $ (oldNumber,newNumber) -- returning just the interval
                                                                                -- if we've not exceeded the max,
                                                                                -- otherwise returning Nothing.
