readReturn :: Char -> Integer
readReturn arr = read (return arr)

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits num = if num > 0 
then map readReturn (show num)
else []

toDigitsRev :: Integer -> [Integer]
toDigitsRev num = reverse (toDigits num)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id,(*2)])

doubleFromsndToLast :: Integer -> [Integer]
doubleFromsndToLast num = doubleEveryOther (toDigitsRev num)

joiner :: [Integer] -> Integer
joiner = read . concatMap show

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits num = foldl (+) 0 (toDigits (joiner num))

-- Exercise 4
validate :: Integer -> Bool
validate num = (sumDigits (doubleFromsndToLast num)) `mod` 10 == 0


-- Exercise 5 Tower of hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
