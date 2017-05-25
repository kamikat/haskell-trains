-- Make a spiral
-- https://www.codewars.com/kata/make-a-spiral
module Spiral where

import Data.List

spiralize :: Int -> [[Int]]
spiralize 1 = [[1]]
spiralize 2 = [[1,1],[0,1]]
spiralize k = roll . transpose . reverse . roll . transpose . reverse $ spiralize (k - 2)
  where roll xxs@(xs:xxs') = (replicate (length xs) 1) : (replicate (length xs - 1) 0 ++ [1]) : xxs

