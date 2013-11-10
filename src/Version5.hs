module Main ( main ) where
import Common


main :: IO ()
main = print =<< maxChain <$> getMaxNumber






maxChain :: Int -> (Int,Int)
maxChain maxNumber = runST $ do 
   lengths   <- newEmptyLengths maxNumber
   globalMax <- newSTRef (1,0)
   mapM_ (collatzLength maxNumber globalMax lengths) [1 .. maxNumber]
   readSTRef globalMax

collatzLength :: Int -> STRef s (Int,Int) -> STUArray s Int Int -> Int -> ST s Int
collatzLength _         _         _       1 = return 0
collatzLength maxNumber globalMax lengths n =
   mfilter (>= 0) <$> safeReadArray lengths n >>=
      (flip maybe return $ do
         len <- (1+) <$> collatzLength maxNumber globalMax lengths (collatz n)
         safeWriteArray lengths n len
         currentMax <- snd <$> readSTRef globalMax
         when (currentMax < len) $ writeSTRef globalMax (n,len)
         return len)
