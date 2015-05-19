module Golf where

-- Exercise 1 - Hopscotch
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (nth xs) [1..(length xs)]

nth :: [a] -> Int -> [a]
nth xs 1 = xs
nth [] _ = []
nth xs n = take 1 (drop (n-1) xs) ++ (nth (drop n xs) (n))

-- Exercise 2 Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) 
 | y > x && y > z = y : (localMaxima (y:(z:xs)))
 | otherwise = localMaxima (y:(z:xs))
localMaxima _ = []
