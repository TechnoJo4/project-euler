import Data.List

-- stolen from stack overflow but i think i understand
sublists :: Int -> [a] -> [[a]]
sublists n xs = take (length xs - n + 1) $ sublists' n xs
    where sublists' _ [] = [[]]
          sublists' n xs@(_:rest) = take n xs : sublists' n rest

main :: IO ()
main = do
    bignumber <- readFile "8.txt"
    print $ foldl1' max $ map (\x -> product $ map (\x -> read [x] :: Int) x) $ sublists 13 bignumber
