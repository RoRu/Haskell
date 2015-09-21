length' :: [a] -> Int
length' [] = 0
length' (_:xl) = 1 + length' xl

append :: [a] -> [a] -> [a]
append [] yl = yl
append (x:xl) yl = x : append xl yl

reverse' :: [a] -> [a]
reverse' = rev [] 
    where
    rev a [] = a
    rev a (x:xl) = rev (x:a) xl

{- rev = fix (\ r a x -> case x of
                        [] -> a
                        (x:xs) -> r (x:a) xs
          ) [] where fix f = let x = f x in x -}

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xl) = x ++ concat xl

intersperse' :: a -> [a] -> [a] 
intersperse' _ ys | length' ys < 2 = ys
intersperse' x (y:ys) = y : x : intersperse' x ys

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x, y):ls) = (x:l1, y:l2) where
    (l1, l2) = unzip' ls

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

isPalindrome :: Eq a => [a] -> Bool
isPalindrome ls = ls == reverse' ls

{-
isPalindrome [] = False
isPalindrome [a] = True
isPalindrome [a,b] = a == b
isPalindrome w = (head w == last w) && isPalindrome middle
  where middle = (init . tail) w
-}

swap :: Int -> Int -> [a] -> [a]
swap i j ls = [get k x | (k, x) <- zip' [0..length ls - 1] ls]
    where get k x | k == i = ls !! j
                  | k == j = ls !! i
                  | otherwise = x

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n ls | n > length' ls = []
           | otherwise = [y | (_, y) <- zip' [0..n-1] ls]

drop' :: Int -> [a] -> [a]
drop' 0 ls = ls
drop' n ls | n > length' ls = []
               | otherwise = reverse' $ take' (length' ls - n) (reverse' ls)

insert :: a -> Int -> [a] -> [a]
insert x n ls 
    | n < 0 || n > length ls = ls
    | otherwise = take n ls ++ [x] ++ drop n ls

dprod :: [a] -> [b] -> [(a, b)]
dprod lx ly = [(x, y) | x <- lx, y <- ly]

{-
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys
-}


perm :: Eq a => [a] -> [[a]]
perm [] = [[]]
perm ls = [x:xs | x <- ls, xs <- perm $ delete x ls]
    where delete e lt = [x | x <- lt, x /= e]


subs :: [Int] -> [[Int]]
subs []  = [[]]
subs (x:xs) = map' (x:) (subs xs) ++ subs xs


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:ls) = f x : map' f ls

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:ls) = f x (foldr' f z ls)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:ls) = let z1 = f z x
                    in foldl' f z1 ls

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]


fix' :: (a -> a) -> a
fix' f = let x = f x in x





-- MERGESORT
split :: [a] -> ([a],[a])
split xs = go xs xs where
  go (x:xl) (_:_:zs) = (x:us,vs) where (us,vs) = go xl zs
  go    xl   _       = ([],xl)

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] ys         = ys
merge _ xs []         = xs
merge pr (x:xs) (y:ys)
    | pr x y = x : merge pr xs (y:ys)
    | otherwise = y : merge pr (x:xs) ys

mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort _ []   = []
mergesort _ [x]  = [x]
mergesort pr xs = merge pr (mergesort pr xs1) (mergesort pr xs2)
  where
    (xs1,xs2) = split xs
-- /MERGESORT



main :: IO()
main = do 
    print . (append [1, 2]) $ [3, 4]
    print $ length' [1..4]
    print $ reverse' [1..4]
    print $ concat' [[1, 2], [3, 4]]
    print (intersperse' ' ' "abc")
    print (zip' "ab" "xyz")
    print $ unzip' [(1, 2), (3, 4)]
    print (zipWith' (+) [1, 2] [3, 4])
    print $ isPalindrome [1, 2, 3, 2, 1]
    print (swap 1 4 [1..6])
    print (take' 2 [1..5])
    print (drop' 3 [1..5])
    print (insert 2 1 [1,3,4,5])
    print (dprod [1, 2] [3..5])
    print (mergesort (<=) [2, 1, 6, 4, 12, 9])
    print (perm [1..3])
    print (map' (+1) [1..3])
    print (foldr' (-) 2 [1..3])
    print (foldl' (-) 2 [1..3])
    print (fibs!!12)
    print (primes!!10)
    print (subs [1..4])