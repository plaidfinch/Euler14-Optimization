module Main ( main ) where
import Common


main :: IO ()
main = print =<< do
   cores  <- getNumCores =<< getNumCapabilities
   config <- Config <$> (newEmptyLengths =<< getCacheSize =<< getMaxNumber)
                    <*> getChunkSize <*> getMaxNumber
                    <*> newTVarIO 1  <*> newTVarIO (1,0)
   mapM_ waitOnFlag =<< (sequence . replicate cores $ spawnWorker config)
   readTVarIO (globalMax config)

maxChainIn :: Config -> (Int,Int) -> IO (Int,Int)
maxChainIn config (low,high) = do
   localMax  <- newIORef (1,0)
   mapM_ (collatzLength config localMax) [low .. high]
   readIORef localMax

collatzLength :: Config -> IORef (Int,Int) -> Int -> IO Int
collatzLength _      _        1 = return 0
collatzLength config localMax n =
   mfilter (>= 0) <$> safeReadArray (lengths config) n >>=
      (flip maybe return $ do
         len <- (1+) <$> collatzLength config localMax (collatz n)
         safeWriteArray (lengths config) n len
         currentMax <- snd <$> readIORef localMax
         when (currentMax < len) $ writeIORef localMax (n,len)
         return len)

data Config = Config { lengths       :: IOUArray Int Int
                     , chunkSize     :: Int
                     , maxNumber     :: Int
                     , currentNumber :: TVar Int
                     , globalMax     :: TVar (Int,Int) }

spawnWorker :: Config -> IO Flag
spawnWorker config =
   (>>) <$> forkIO . worker <*> return =<< newFlag
   where worker flag = getWorkChunk config >>= \c -> case c of
            Nothing -> raiseFlag flag
            Just interval -> do
               (n,len) <- maxChainIn config interval
               atomically $ do
                  currentMax <- snd <$> readTVar (globalMax config)
                  when (currentMax < len) $ writeTVar (globalMax config) (n,len)
               worker flag

type Flag  = MVar ()
newFlag    = newEmptyMVar
raiseFlag  = flip putMVar ()
waitOnFlag = takeMVar

getWorkChunk :: Config -> IO (Maybe (Int,Int))
getWorkChunk config = atomically $ do
   oldNumber <- readTVar (currentNumber config)
   let newNumber = min <$> maxNumber <*> (oldNumber +) . chunkSize $ config
   writeTVar (currentNumber config) (newNumber + 1)
   return . mfilter (fst <**> pure (<=) <*> snd) . Just $ (oldNumber,newNumber)
