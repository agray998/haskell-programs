fizzbuzz :: Int -> [Char]
fizzbuzz n | ((n `mod` 3 == 0) && (n `mod` 5 == 0)) = "FizzBuzz" 
           | ((n `mod` 3 == 0) && (n `mod` 5 /= 0)) = "Fizz" 
           | ((n `mod` 3 /= 0) && (n `mod` 5 == 0)) = "Buzz" 
           | otherwise = show n 

loop i = do
  print $ fizzbuzz i
  let a = i + 1
  if a < 100 then loop a else print "done"

main = do
  loop 1
