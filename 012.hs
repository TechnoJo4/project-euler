import Primes

triangulars :: [Int]
triangulars = drop 1 $ scanl (+) 0 [1..]

divisors :: Int -> [(Int,Int)]
divisors n = foldl divisors' [] [1..(ceiling $ sqrt $ fromIntegral n)]
        where divisors' l prime = let (divp, modp) = n `divMod` prime in
                if modp == 0 && prime < divp then (prime, divp) : l else l

main :: IO ()
main = print $ last $ head $ dropWhile (\l -> length l < 500)
        $ map (concatMap (\(a,b) -> if a == b then [a] else [a,b]))
        $ map divisors triangulars
