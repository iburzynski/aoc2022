{-# LANGUAGE ScopedTypeVariables #-}

module D01 where

import Data.List (insertBy)

type CurrentTotal = Int

d01_1 :: [String] -> String
d01_1 = show . worker max 0

d01_2 :: [String] -> String
d01_2 = show . sum . worker max3 []
 where max3 n = take 3 . insertBy (flip compare) n

worker :: forall acc . (CurrentTotal -> acc -> acc) -> acc -> [String] -> acc
worker calc i = uncurry calc . foldr reducer (0, i)
-- calc must be applied to final results of fold so the final group's total is accounted for
  where
    reducer :: String -> (CurrentTotal, acc) -> (CurrentTotal, acc)
    -- reset current total and calculate new accumulator or increase current total
    reducer str (c, a) = if null str then (0, calc c a) else (c + read str, a)