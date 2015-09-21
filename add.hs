inc :: Int -> Int
dec :: Int -> Int
inc = (+1) 
dec = subtract 1
add :: Int -> Int -> Int
add x 
    | x == 0 = id
    | x > 0 = inc . add (dec x)
    | otherwise = add (inc x) . dec
-- (add 2) 7

main :: IO()
main = print (add 5 7)