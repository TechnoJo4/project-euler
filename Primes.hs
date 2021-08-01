module Primes where

primes :: [Int]
primes = 2 : 3 : 5 : (drop 1 $ filter (\n -> not
        $ any (\x -> n `mod` x == 0)
        $ takeWhile (<= (ceiling $ sqrt $ fromIntegral n)) primes)
        [4..])

factorize :: Int -> [Int]
factorize n = let maxfactor = ceiling $ sqrt $ fromIntegral n in
        foldl factorize' [n] $ takeWhile (<= maxfactor) primes
        where factorize' l@(x:xs) prime = let (divp, modp) = x `divMod` prime in
                if modp == 0 then factorize' (divp:prime:xs) prime else l
