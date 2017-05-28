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

lastDigit' []     = 1
lastDigit' (x:xs) = case xs of (1:_)   -> lookup 1
                               (0:a:_) -> if a /= 0 then 1 else lookup e
                               (a:b:_) -> if b /= 0 then fastLook ds a else lookup e
                               _       -> lookup e
  where e  = lastDigit' xs
        x' = if x < 20 then x else x `mod` 20 + 20
        ds = digitLookup !! (fromIntegral x')
        lookup a = case a of 0 -> 1
                             1 -> x'
                             j -> ds !! (fromIntegral $ j `mod` 4)
        fastLook (a:b:c:_) e'
          | a == b    = a
          | a == c    = if e' `mod` 2 == 0 then a else b
          | otherwise = lookup e
