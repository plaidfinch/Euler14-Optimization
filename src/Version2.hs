module Main ( main ) where
import Common


main :: IO ()
main = print =<< maxChain <$> getMaxNumber







maxChain :: Int -> (Int,Int)
maxChain maxNumber =
   maximumBy (comparing snd)  $                               -- Find the pair with the largest snd element
      map  (\n -> (,) n  $  collatzLength n) [1 .. maxNumber] -- in all pairs of (n, length of chain at n).

-- We factor out the length calculation into a separate function.
collatzLength :: Int -> Int         -- Because we're not building full lists of Collatz chains, it's faster.
collatzLength 1 = 0                                            -- A chain starting at 1 is length 0.
collatzLength n = 1 + collatzLength (collatz n)                -- Any other chain is 1 + length of the rest.
