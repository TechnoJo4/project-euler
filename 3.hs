primes :: Int -> [Int]
primes n = foldl (\acc x -> out x acc) [2..n] [2..(ceiling $ sqrt $ fromIntegral n)]
    where out d xs = filter (\x -> x <= d || (x `mod` d) /= 0) xs

factorize :: Int -> [Int]
factorize n = snd $ foldl out (n, []) $ primes $ ceiling $ sqrt $ fromIntegral n
    where out (n, factors) prime = if (n `mod` prime) == 0
            then out (n `div` prime, prime : factors) prime
            else (n, factors)

main :: IO ()
main = print $ maximum (factorize 600851475143)
