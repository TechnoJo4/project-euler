-- copied from my solution of problem 3
primes :: Int -> [Int]
primes n = foldl (\acc x -> out x acc) [2..n] [2..(ceiling $ sqrt $ fromIntegral n)]
    where out d xs = filter (\x -> x <= d || (x `mod` d) /= 0) xs

main :: IO ()
main = print $ primes 1000000000 !! 10000
-- ridiculously large n cause i don't care
