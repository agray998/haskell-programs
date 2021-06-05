fizzbuzz :: Int -> [Char]
fizzbuzz n | ((n `mod` 3 == 0) && (n `mod` 5 == 0)) = "FizzBuzz" 
           | ((n `mod` 3 == 0) && (n `mod` 5 /= 0)) = "Fizz" 
           | ((n `mod` 3 /= 0) && (n `mod` 5 == 0)) = "Buzz" 
           | otherwise = show n 

loop i n = do
  print $ fizzbuzz i
  let a = i + 1
  if a <= n then loop a n else print "done"

main = do
  loop 1 100
