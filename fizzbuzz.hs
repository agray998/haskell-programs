fizzbuzz :: Int -> [Char]
fizzbuzz n | ((n `mod` 3 == 0) && (n `mod` 5 == 0)) = "FizzBuzz" 
           | ((n `mod` 3 == 0) && (n `mod` 5 /= 0)) = "Fizz" 
           | ((n `mod` 3 /= 0) && (n `mod` 5 == 0)) = "Buzz" 
           | otherwise = show n 


main = print ([fizzbuzz n | n <- [1..100]])
