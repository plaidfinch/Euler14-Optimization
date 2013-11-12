module Main ( main ) where
import Common
import MutableArray

main :: IO ()
main = print =<< maxChain <$> getMaxNumber






maxChain :: Int -> (Int,Int)
maxChain maxNumber = runST $ do
   lengths   <- newEmptyLengths maxNumber
   maximumBy (comparing snd) <$>
      mapM (\n -> (,) n <$> collatzLength maxNumber lengths n) [1 .. maxNumber]


collatzLength :: Int -> STUArray s Int Int -> Int -> ST s Int
collatzLength _         _       1 = return 0
collatzLength maxNumber lengths n =
   mfilter (>= 0) <$> safeReadArray lengths n >>=
      (flip maybe return $ do
         len <- (1+) <$> collatzLength maxNumber lengths (collatz n)
         safeWriteArray lengths n len
         return len)
