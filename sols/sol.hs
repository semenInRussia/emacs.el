module SupermarketQueue
  ( queueTime
  ) where

import Data.List (sort)

queueTime' :: [Int] -> [Int] -> Int
queueTime' [] ws = last ws
queueTime' (x:xs) ws = queueTime' xs $ checkout x ws
  where
    checkout a (w:ws') = sort $ (a + w) : ws'
    checkout _ [] = []

queueTime :: [Int] -> Int -> Int
queueTime customers n = queueTime' customers (createWorkers n)
  where
    createWorkers n' = replicate n' 0
