module Main ( main ) where
import Common
import MutableArray

main :: IO ()
main = print =<< maxChain <$> getMaxNumber






maxChain :: Int -> (Int,Int)
maxChain maxNumber = runST $ do            -- In the ST monad,
   lengths   <- newEmptyLengths maxNumber  -- make a new lengths array (see MutableArray.hs),
   maximumBy (comparing snd) <$>           -- and then the rest of this is the same as w/ the State monad...
      mapM (\n -> (,) n <$> collatzLength maxNumber lengths n) [1 .. maxNumber]

-- We exchange the State monad for the safe-single-threaded-imperative-state monad ST.
collatzLength :: Int -> STUArray s Int Int -> Int -> ST s Int
collatzLength _         _       1 = return 0
collatzLength maxNumber lengths n =
   mfilter (>= 0) <$> safeReadArray lengths n >>=                    -- We use an STArray rather than a map,
      (flip maybe return $ do                                        -- but thanks to the monad abstraction,
         len <- (1+) <$> collatzLength maxNumber lengths (collatz n) -- everything else is identical.
         safeWriteArray lengths n len
         return len)
