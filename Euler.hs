euler :: Int -> Int
euler n = length [x | x <- [1..abs n - 1], gcd n x == 1]

main :: IO()
main = print $ euler 12
