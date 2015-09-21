gcd' :: Int -> Int -> Int
gcd' x y | y == 0    = x
         | otherwise = gcd' y (mod x y)

lcm' :: Int -> Int -> Int
lcm' a b = div a (gcd' a b) * b

totient :: Int -> Int
totient n = length [x | x <- [1..n-1], gcd' n x == 1]

isprime :: Int -> Bool
isprime n = totient n == n - 1

coprime :: Int -> Int -> Bool
coprime n m = gcd' n m == 1

nd :: Int -> Int
nd n
    | n == 0 = error "undefined"
    | otherwise = length [x | x <- [2..n `div` 2], n `mod` x == 0]

sd :: Int -> Int
sd n
    | n == 0 = error "undefined"
    | n > 0 = sum [x | x <- [2..n `div` 2], n `mod` x == 0]
    | otherwise = 0

main :: IO()
main = do
    print $ gcd' 4 6
    print $ lcm' 4 6
    print $ totient 10
    print $ isprime 11
    print $ nd 8
    print $ sd 8
    print $ coprime 25 5
