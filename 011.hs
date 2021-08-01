import Data.List

try grid x y = (
        if can_x
            then product $ take 4 $ drop x $ grid !! y
            else 0,
        if can_y
            then product $ map (!! x) $ take 4 $ drop y $ grid
            else 0,
        if can_x && can_y
            then product $ snd $ mapAccumL (\acc xs -> (acc+1, xs !! acc)) 0 $ map (drop x) $ take 4 $ drop y $ grid 
            else 0,
        if can_x && y >= 3
            then product $ snd $ mapAccumL (\acc xs -> (acc-1, xs !! acc)) 3 $ map (drop x) $ take 4 $ drop (y-3) $ grid 
            else 0) where
    can_x = x <= (length $ head grid) - 4
    can_y = y <= (length grid) - 4

main :: IO ()
main = do
    gridstr <- readFile "011.txt"
    let grid = map (map (read :: String -> Int) . words) $ lines gridstr

    let products = [try grid x y | x <- [0..19], y <- [0..19]]
    print $ products

    print $ maximum $ map (\(a,b,c,d) -> max a $ max b $ max c d) products
