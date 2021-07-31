module Primes where

primes :: [Int]
primes = 2 : 3 : 5 : (drop 1 $ filter (\n -> not
        $ any (\x -> n `mod` x == 0)
        $ takeWhile (<= (ceiling $ sqrt $ fromIntegral n)) primes)
        [4..])
