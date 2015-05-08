module Golf where

-- Exercise 1
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (nth xs) [1..(length xs)]

nth :: [a] -> Int -> [a]
nth xs 1 = xs
nth [] _ = []
nth xs n = take 1 (drop (n-1) xs) ++ (nth (drop n xs) (n))
