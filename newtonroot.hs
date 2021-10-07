-- A function to calculate the square root of a number n according to Newton's method for calculating square roots

absolute n | n < 0 = -n
           | otherwise = n

avg a b = (a + b) / 2

newtonRoot n i | absolute (i - (n / i)) < 0.001 = i
               | otherwise = newtonRoot n (avg i (n / i))

main = do
    print $ newtonRoot 100 12