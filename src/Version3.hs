module Main ( main ) where
import Common
import qualified Data.IntMap as M

main :: IO ()
main = print =<< maxChain <$> getMaxNumber







maxChain :: Int -> (Int,Int)
maxChain maxNumber = flip evalState M.empty $
   maximumBy (comparing snd) <$>
      mapM (\n -> (,) n <$> collatzLength maxNumber n) [1 .. maxNumber]


collatzLength :: Int -> Int -> State (IntMap Int) Int
collatzLength _         1 = return 0
collatzLength maxNumber n =
   gets (M.lookup n) >>=
      (flip maybe return $ do
         len <- (1+) <$> collatzLength maxNumber (collatz n)
         when (n <= maxNumber) $ modify (M.insert n len)
         return len)
