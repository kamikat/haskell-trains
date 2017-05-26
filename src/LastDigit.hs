-- Last digit of a huge number
-- https://www.codewars.com/kata/last-digit-of-a-huge-number
module LastDigit where

lastDigit :: [Integer] -> Integer
lastDigit = flip mod 10 . lastDigit'

-- digitLookup = [ [ let p = x ^ k
--                       m = p `mod` 20
--                    in if p < 20 then m
--                                 else m + 20 -- carry flag
--                 | k <- [4..7] ]
--               | x <- [0..39] ]

digitLookup = [
  [0,0,0,0],     [1,1,1,1],     [16,32,24,28], [21,23,29,27], [36,24,36,24], [25,25,25,25],
                 [36,36,36,36], [21,27,29,23], [36,28,24,32], [21,29,21,29],
  [20,20,20,20], [21,31,21,31], [36,32,24,28], [21,33,29,37], [36,24,36,24], [25,35,25,35],
                 [36,36,36,36], [21,37,29,33], [36,28,24,32], [21,39,21,39],
  [20,20,20,20], [21,21,21,21], [36,32,24,28], [21,23,29,27], [36,24,36,24], [25,25,25,25],
                 [36,36,36,36], [21,27,29,23], [36,28,24,32], [21,29,21,29],
  [20,20,20,20], [21,31,21,31], [36,32,24,28], [21,33,29,37], [36,24,36,24], [25,35,25,35],
                 [36,36,36,36], [21,37,29,33], [36,28,24,32], [21,39,21,39]]

lastDigit' [] = 1
lastDigit' (x:xs) = let x' = if x < 20 then x else x `mod` 20 + 20
                        ds = digitLookup !! (fromIntegral x')
                        ex = case xs of (1:_)   -> 1                -- ex is 1 when xs is [1, ...]
                                        (0:x:_) -> if x /= 0 then 0 -- ex is 0 when xs is [0, (non-zero), ...]
                                                             else lastDigit' xs
                                        _       -> lastDigit' xs
                     in case ex of 0 -> 1
                                   1 -> x
                                   j -> ds !! (fromIntegral $ j `mod` 4)
