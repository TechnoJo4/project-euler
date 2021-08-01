import Data.List
import Data.Function

collatz :: Int -> Int
collatz n = if even n then n `div` 2 else 3*n + 1

main :: IO ()
main = print $ maximumBy (compare `on` snd)
        $ map (\(n,s) -> (n,length s))
        [(,) n $ takeWhile (/= 1) $ iterate collatz n | n <- [1..1000000]]
