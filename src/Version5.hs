module Main ( main ) where
import Common
import MutableArray

main :: IO ()
main = print =<< maxChain <$> getMaxNumber






maxChain :: Int -> (Int,Int)
maxChain maxNumber = runST $ do                       -- In the ST monad,
   lengths   <- newEmptyLengths maxNumber             -- make a new lengths array (see MutableArray.hs)
   globalMax <- newSTRef (1,0)                        -- and a new mutable ref to hold the maximum,
   mapM_ (collatzLength maxNumber globalMax lengths) [1 .. maxNumber] -- then find every chain length
   readSTRef globalMax                                                -- and read off the global max.

collatzLength :: Int -> STRef s (Int,Int) -> STUArray s Int Int -> Int -> ST s Int
collatzLength _         _         _       1 = return 0
collatzLength maxNumber globalMax lengths n =              -- Everything is the same as Version4, except:
   mfilter (>= 0) <$> safeReadArray lengths n >>=          
      (flip maybe return $ do
         len <- (1+) <$> collatzLength maxNumber globalMax lengths (collatz n)
         safeWriteArray lengths n len
         currentMax <- snd <$> readSTRef globalMax              -- now we grab the max from the global ref,
         when (currentMax < len) $ writeSTRef globalMax (n,len) -- and update it if our value is higher.
         return len)                                            -- This is more efficient, because we eliminate
                                                                -- the intermediate list of chain lengths.
