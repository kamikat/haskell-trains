-- Conway's Game of Life - Unlimited Edition
-- https://www.codewars.com/kata/52423db9add6f6fc39000354
module UnlimitedGameOfLife where

import Data.List

addPadding :: [[Int]] -> [[Int]]
addPadding [] = replicate 3 . replicate 3 $ 0
addPadding xxs@(r:rs) = map hpad . vpad $ xxs
  where width     = length r
        height    = length xxs
        dimension = max width height + 4
        paddingTop    = 2
        paddingBottom = dimension - height - paddingTop
        paddingLeft   = 2
        paddingRight  = dimension - width - paddingLeft
        zeros    = replicate width 0
        vpad xxs = (replicate paddingTop zeros) ++ xxs ++ (replicate paddingBottom zeros)
        hpad xs  = (replicate paddingLeft 0) ++ xs ++ (replicate paddingRight 0)

crop :: [[Int]] -> [[Int]]
crop = rotate . skipCondition . rotate . skipCondition . rotate . skipCondition . rotate . skipCondition
  where skipCondition = dropWhile (all (==0))
        rotate = reverse . transpose

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration xxs 0 = xxs
getGeneration xxs k = crop $ getGeneration (nextGeneration $ addPadding xxs) (k - 1)
  where getCellState 1 2 = 1 -- Rule 3
        getCellState _ 3 = 1 -- Rule 3,4
        getCellState _ _ = 0 -- Otherwise.
        nextGeneration  (_:_:[])      = []
        nextGeneration  (y1:y2:y3:ys) = (nextGeneration' y1 y2 y3) : (nextGeneration $ y2:y3:ys)
        nextGeneration' (_:_:[]) (_:_:[]) (_:_:[]) = []
        nextGeneration' (x11:x12:x13:x1s)
                        (x21:x22:x23:x2s)
                        (x31:x32:x33:x3s) = (getCellState x22 $ x11 + x12 + x13 + x21 + x23 + x31 + x32 + x33) : nextGeneration' (x12:x13:x1s) (x22:x23:x2s) (x32:x33:x3s)
