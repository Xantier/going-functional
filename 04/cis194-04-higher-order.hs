-- Exercise 1 - Wholemeal programming

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2step :: Integer -> Integer
fun2step n
	| even n = n `div` 2
	| otherwise = 3 * n + 1

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate fun2step
