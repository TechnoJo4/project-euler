main :: IO ()
main = print
        $ take 1
        $ filter (\(a,b,c) -> a+b+c == 1000)
        [(a, b, truncate c) |
            a <- [1..], b <- [1..], a < b,
            let c = sqrt $ fromIntegral (a^2 + b^2),
            c == fromIntegral (truncate c)]
