module Main ( main ) where
import Common


main :: IO ()
main = print =<< maxChain <$> getMaxNumber







maxChain :: Int -> (Int,Int)
maxChain maxNumber = (head &&& length) .                        -- Get the first element & length
   maximumBy (comparing length) $                               -- of the maximal-length list
      map (takeWhile (/= 1) . iterate collatz) [1 .. maxNumber] -- in all lists of Collatz chains.
