module Main ( main ) where
import Common
import qualified Data.IntMap as M

main :: IO ()
main = print =<< maxChain <$> getMaxNumber







maxChain :: Int -> (Int,Int)
maxChain maxNumber = flip evalState M.empty $         -- Run the State computation with an initially empty map,
   maximumBy (comparing snd) <$>                      -- and find the element with the max snd element in
      mapM (\n -> (,) n <$> collatzLength maxNumber n) [1 .. maxNumber] -- all pairs of (n, chain-length)

-- We use the State monad to memoize the computation, storing each maximum in an IntMap.
collatzLength :: Int -> Int -> State (IntMap Int) Int
collatzLength _         1 = return 0                         -- The length of the chain at 1 is 0.
collatzLength maxNumber n =
   gets (M.lookup n) >>=                                     -- If n is in the map,
      (flip maybe return $ do                                -- return it; otherwise
         len <- (1+) <$> collatzLength maxNumber (collatz n) -- get the length of the chain at n
         when (n <= maxNumber) $ modify (M.insert n len)     -- and if n < maxNumber, remember this length
         return len)                                         -- and finally, return the length.
