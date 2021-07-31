-- this fib shamelessly stolen from wikipedia
fib :: [Int]
fib = 0 : scanl (+) 1 fib

main :: IO ()
main = print $ sum $ filter even $ takeWhile (<=4000000) fib
