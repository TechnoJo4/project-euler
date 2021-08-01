main :: IO ()
main = do
    numbers <- readFile "013.txt"
    print $ sum (map read $ lines numbers :: [Integer])
