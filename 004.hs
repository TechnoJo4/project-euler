main :: IO ()
main = print $ maximum [x | a <- [100..999], b <- [100..999], let x = a*b, show x == (reverse $ show x)]
