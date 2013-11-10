module Main ( main ) where
import Common


main :: IO ()
main = print =<< maxChain <$> getMaxNumber







maxChain :: Int -> (Int,Int)
maxChain maxNumber = (head &&& length) .
   maximumBy (comparing length) $
      map (takeWhile (/= 1) . iterate collatz) [1 .. maxNumber]
