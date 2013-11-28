module Main ( main ) where
import Common


main :: IO ()
main = print =<< maxChain <$> getMaxNumber







maxChain :: Int -> (Int,Int)
maxChain maxNumber = (head &&& length) . -- get the first element and length of ...
   maximumBy (comparing length) $        -- ...the maximal-length list in...
      map (takeWhile (/= 1) . iterate collatz) [1 .. maxNumber] -- ...all Collatz chains
