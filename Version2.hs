module Main ( main ) where
import Common


main :: IO ()
main = print =<< maxChain <$> getMaxNumber







maxChain :: Int -> (Int,Int)
maxChain maxNumber =
   maximumBy (comparing snd)  $
      map  (\n -> (,) n  $  collatzLength n) [1 .. maxNumber]


collatzLength :: Int -> Int
collatzLength 1 = 0
collatzLength n = 1 + collatzLength (collatz n)
